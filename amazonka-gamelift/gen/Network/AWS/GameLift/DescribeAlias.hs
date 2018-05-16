{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for an alias. This operation returns all alias metadata and settings. To get an alias's target fleet ID only, use @ResolveAlias@ .
--
--
-- To get alias properties, specify the alias ID. If successful, the requested alias record is returned.
--
-- Alias-related operations include:
--
--     * 'CreateAlias'
--
--     * 'ListAliases'
--
--     * 'DescribeAlias'
--
--     * 'UpdateAlias'
--
--     * 'DeleteAlias'
--
--     * 'ResolveAlias'
--
--
--
module Network.AWS.GameLift.DescribeAlias
    (
    -- * Creating a Request
      describeAlias
    , DescribeAlias
    -- * Request Lenses
    , dAliasId

    -- * Destructuring the Response
    , describeAliasResponse
    , DescribeAliasResponse
    -- * Response Lenses
    , darsAlias
    , darsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'describeAlias' smart constructor.
newtype DescribeAlias = DescribeAlias'
  { _dAliasId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAliasId' - Unique identifier for a fleet alias. Specify the alias you want to retrieve.
describeAlias
    :: Text -- ^ 'dAliasId'
    -> DescribeAlias
describeAlias pAliasId_ = DescribeAlias' {_dAliasId = pAliasId_}


-- | Unique identifier for a fleet alias. Specify the alias you want to retrieve.
dAliasId :: Lens' DescribeAlias Text
dAliasId = lens _dAliasId (\ s a -> s{_dAliasId = a})

instance AWSRequest DescribeAlias where
        type Rs DescribeAlias = DescribeAliasResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAliasResponse' <$>
                   (x .?> "Alias") <*> (pure (fromEnum s)))

instance Hashable DescribeAlias where

instance NFData DescribeAlias where

instance ToHeaders DescribeAlias where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeAlias" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAlias where
        toJSON DescribeAlias'{..}
          = object (catMaybes [Just ("AliasId" .= _dAliasId)])

instance ToPath DescribeAlias where
        toPath = const "/"

instance ToQuery DescribeAlias where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'describeAliasResponse' smart constructor.
data DescribeAliasResponse = DescribeAliasResponse'
  { _darsAlias          :: !(Maybe Alias)
  , _darsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsAlias' - Object that contains the requested alias.
--
-- * 'darsResponseStatus' - -- | The response status code.
describeAliasResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DescribeAliasResponse
describeAliasResponse pResponseStatus_ =
  DescribeAliasResponse'
    {_darsAlias = Nothing, _darsResponseStatus = pResponseStatus_}


-- | Object that contains the requested alias.
darsAlias :: Lens' DescribeAliasResponse (Maybe Alias)
darsAlias = lens _darsAlias (\ s a -> s{_darsAlias = a})

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeAliasResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DescribeAliasResponse where
