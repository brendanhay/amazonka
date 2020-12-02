{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeConnectionAliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the connection aliases used for cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.DescribeConnectionAliases
  ( -- * Creating a Request
    describeConnectionAliases,
    DescribeConnectionAliases,

    -- * Request Lenses
    dcaResourceId,
    dcaAliasIds,
    dcaNextToken,
    dcaLimit,

    -- * Destructuring the Response
    describeConnectionAliasesResponse,
    DescribeConnectionAliasesResponse,

    -- * Response Lenses
    desrsConnectionAliases,
    desrsNextToken,
    desrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'describeConnectionAliases' smart constructor.
data DescribeConnectionAliases = DescribeConnectionAliases'
  { _dcaResourceId ::
      !(Maybe Text),
    _dcaAliasIds :: !(Maybe (List1 Text)),
    _dcaNextToken :: !(Maybe Text),
    _dcaLimit :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeConnectionAliases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcaResourceId' - The identifier of the directory associated with the connection alias.
--
-- * 'dcaAliasIds' - The identifiers of the connection aliases to describe.
--
-- * 'dcaNextToken' - If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- * 'dcaLimit' - The maximum number of connection aliases to return.
describeConnectionAliases ::
  DescribeConnectionAliases
describeConnectionAliases =
  DescribeConnectionAliases'
    { _dcaResourceId = Nothing,
      _dcaAliasIds = Nothing,
      _dcaNextToken = Nothing,
      _dcaLimit = Nothing
    }

-- | The identifier of the directory associated with the connection alias.
dcaResourceId :: Lens' DescribeConnectionAliases (Maybe Text)
dcaResourceId = lens _dcaResourceId (\s a -> s {_dcaResourceId = a})

-- | The identifiers of the connection aliases to describe.
dcaAliasIds :: Lens' DescribeConnectionAliases (Maybe (NonEmpty Text))
dcaAliasIds = lens _dcaAliasIds (\s a -> s {_dcaAliasIds = a}) . mapping _List1

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
dcaNextToken :: Lens' DescribeConnectionAliases (Maybe Text)
dcaNextToken = lens _dcaNextToken (\s a -> s {_dcaNextToken = a})

-- | The maximum number of connection aliases to return.
dcaLimit :: Lens' DescribeConnectionAliases (Maybe Natural)
dcaLimit = lens _dcaLimit (\s a -> s {_dcaLimit = a}) . mapping _Nat

instance AWSRequest DescribeConnectionAliases where
  type
    Rs DescribeConnectionAliases =
      DescribeConnectionAliasesResponse
  request = postJSON workSpaces
  response =
    receiveJSON
      ( \s h x ->
          DescribeConnectionAliasesResponse'
            <$> (x .?> "ConnectionAliases")
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeConnectionAliases

instance NFData DescribeConnectionAliases

instance ToHeaders DescribeConnectionAliases where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.DescribeConnectionAliases" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeConnectionAliases where
  toJSON DescribeConnectionAliases' {..} =
    object
      ( catMaybes
          [ ("ResourceId" .=) <$> _dcaResourceId,
            ("AliasIds" .=) <$> _dcaAliasIds,
            ("NextToken" .=) <$> _dcaNextToken,
            ("Limit" .=) <$> _dcaLimit
          ]
      )

instance ToPath DescribeConnectionAliases where
  toPath = const "/"

instance ToQuery DescribeConnectionAliases where
  toQuery = const mempty

-- | /See:/ 'describeConnectionAliasesResponse' smart constructor.
data DescribeConnectionAliasesResponse = DescribeConnectionAliasesResponse'
  { _desrsConnectionAliases ::
      !( Maybe
           ( List1
               ConnectionAlias
           )
       ),
    _desrsNextToken ::
      !(Maybe Text),
    _desrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeConnectionAliasesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsConnectionAliases' - Information about the specified connection aliases.
--
-- * 'desrsNextToken' - The token to use to retrieve the next set of results, or null if no more results are available.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeConnectionAliasesResponse ::
  -- | 'desrsResponseStatus'
  Int ->
  DescribeConnectionAliasesResponse
describeConnectionAliasesResponse pResponseStatus_ =
  DescribeConnectionAliasesResponse'
    { _desrsConnectionAliases =
        Nothing,
      _desrsNextToken = Nothing,
      _desrsResponseStatus = pResponseStatus_
    }

-- | Information about the specified connection aliases.
desrsConnectionAliases :: Lens' DescribeConnectionAliasesResponse (Maybe (NonEmpty ConnectionAlias))
desrsConnectionAliases = lens _desrsConnectionAliases (\s a -> s {_desrsConnectionAliases = a}) . mapping _List1

-- | The token to use to retrieve the next set of results, or null if no more results are available.
desrsNextToken :: Lens' DescribeConnectionAliasesResponse (Maybe Text)
desrsNextToken = lens _desrsNextToken (\s a -> s {_desrsNextToken = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeConnectionAliasesResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\s a -> s {_desrsResponseStatus = a})

instance NFData DescribeConnectionAliasesResponse
