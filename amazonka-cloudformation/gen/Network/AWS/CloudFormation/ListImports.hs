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
-- Module      : Network.AWS.CloudFormation.ListImports
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all stacks that are importing an exported output value. To modify or remove an exported output value, first use this action to see which stacks are using it. To see the exported output values in your account, see 'ListExports' .
--
--
-- For more information about importing an exported output value, see the <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-importvalue.html @Fn::ImportValue@ > function.
--
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListImports
    (
    -- * Creating a Request
      listImports
    , ListImports
    -- * Request Lenses
    , liNextToken
    , liExportName

    -- * Destructuring the Response
    , listImportsResponse
    , ListImportsResponse
    -- * Response Lenses
    , lirsImports
    , lirsNextToken
    , lirsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listImports' smart constructor.
data ListImports = ListImports'
  { _liNextToken  :: !(Maybe Text)
  , _liExportName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListImports' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liNextToken' - A string (provided by the 'ListImports' response output) that identifies the next page of stacks that are importing the specified exported output value.
--
-- * 'liExportName' - The name of the exported output value. AWS CloudFormation returns the stack names that are importing this value.
listImports
    :: Text -- ^ 'liExportName'
    -> ListImports
listImports pExportName_ =
  ListImports' {_liNextToken = Nothing, _liExportName = pExportName_}


-- | A string (provided by the 'ListImports' response output) that identifies the next page of stacks that are importing the specified exported output value.
liNextToken :: Lens' ListImports (Maybe Text)
liNextToken = lens _liNextToken (\ s a -> s{_liNextToken = a})

-- | The name of the exported output value. AWS CloudFormation returns the stack names that are importing this value.
liExportName :: Lens' ListImports Text
liExportName = lens _liExportName (\ s a -> s{_liExportName = a})

instance AWSPager ListImports where
        page rq rs
          | stop (rs ^. lirsNextToken) = Nothing
          | stop (rs ^. lirsImports) = Nothing
          | otherwise =
            Just $ rq & liNextToken .~ rs ^. lirsNextToken

instance AWSRequest ListImports where
        type Rs ListImports = ListImportsResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "ListImportsResult"
              (\ s h x ->
                 ListImportsResponse' <$>
                   (x .@? "Imports" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListImports where

instance NFData ListImports where

instance ToHeaders ListImports where
        toHeaders = const mempty

instance ToPath ListImports where
        toPath = const "/"

instance ToQuery ListImports where
        toQuery ListImports'{..}
          = mconcat
              ["Action" =: ("ListImports" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "NextToken" =: _liNextToken,
               "ExportName" =: _liExportName]

-- | /See:/ 'listImportsResponse' smart constructor.
data ListImportsResponse = ListImportsResponse'
  { _lirsImports        :: !(Maybe [Text])
  , _lirsNextToken      :: !(Maybe Text)
  , _lirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListImportsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lirsImports' - A list of stack names that are importing the specified exported output value.
--
-- * 'lirsNextToken' - A string that identifies the next page of exports. If there is no additional page, this value is null.
--
-- * 'lirsResponseStatus' - -- | The response status code.
listImportsResponse
    :: Int -- ^ 'lirsResponseStatus'
    -> ListImportsResponse
listImportsResponse pResponseStatus_ =
  ListImportsResponse'
    { _lirsImports = Nothing
    , _lirsNextToken = Nothing
    , _lirsResponseStatus = pResponseStatus_
    }


-- | A list of stack names that are importing the specified exported output value.
lirsImports :: Lens' ListImportsResponse [Text]
lirsImports = lens _lirsImports (\ s a -> s{_lirsImports = a}) . _Default . _Coerce

-- | A string that identifies the next page of exports. If there is no additional page, this value is null.
lirsNextToken :: Lens' ListImportsResponse (Maybe Text)
lirsNextToken = lens _lirsNextToken (\ s a -> s{_lirsNextToken = a})

-- | -- | The response status code.
lirsResponseStatus :: Lens' ListImportsResponse Int
lirsResponseStatus = lens _lirsResponseStatus (\ s a -> s{_lirsResponseStatus = a})

instance NFData ListImportsResponse where
