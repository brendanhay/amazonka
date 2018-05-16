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
-- Module      : Network.AWS.CloudFormation.ListExports
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all exported output values in the account and region in which you call this action. Use this action to see the exported output values that you can import into other stacks. To import values, use the <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-importvalue.html @Fn::ImportValue@ > function.
--
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-exports.html AWS CloudFormation Export Stack Output Values> .
--
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListExports
    (
    -- * Creating a Request
      listExports
    , ListExports
    -- * Request Lenses
    , leNextToken

    -- * Destructuring the Response
    , listExportsResponse
    , ListExportsResponse
    -- * Response Lenses
    , lersNextToken
    , lersExports
    , lersResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listExports' smart constructor.
newtype ListExports = ListExports'
  { _leNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListExports' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leNextToken' - A string (provided by the 'ListExports' response output) that identifies the next page of exported output values that you asked to retrieve.
listExports
    :: ListExports
listExports = ListExports' {_leNextToken = Nothing}


-- | A string (provided by the 'ListExports' response output) that identifies the next page of exported output values that you asked to retrieve.
leNextToken :: Lens' ListExports (Maybe Text)
leNextToken = lens _leNextToken (\ s a -> s{_leNextToken = a})

instance AWSPager ListExports where
        page rq rs
          | stop (rs ^. lersNextToken) = Nothing
          | stop (rs ^. lersExports) = Nothing
          | otherwise =
            Just $ rq & leNextToken .~ rs ^. lersNextToken

instance AWSRequest ListExports where
        type Rs ListExports = ListExportsResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "ListExportsResult"
              (\ s h x ->
                 ListExportsResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Exports" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ListExports where

instance NFData ListExports where

instance ToHeaders ListExports where
        toHeaders = const mempty

instance ToPath ListExports where
        toPath = const "/"

instance ToQuery ListExports where
        toQuery ListExports'{..}
          = mconcat
              ["Action" =: ("ListExports" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "NextToken" =: _leNextToken]

-- | /See:/ 'listExportsResponse' smart constructor.
data ListExportsResponse = ListExportsResponse'
  { _lersNextToken      :: !(Maybe Text)
  , _lersExports        :: !(Maybe [Export])
  , _lersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListExportsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lersNextToken' - If the output exceeds 100 exported output values, a string that identifies the next page of exports. If there is no additional page, this value is null.
--
-- * 'lersExports' - The output for the 'ListExports' action.
--
-- * 'lersResponseStatus' - -- | The response status code.
listExportsResponse
    :: Int -- ^ 'lersResponseStatus'
    -> ListExportsResponse
listExportsResponse pResponseStatus_ =
  ListExportsResponse'
    { _lersNextToken = Nothing
    , _lersExports = Nothing
    , _lersResponseStatus = pResponseStatus_
    }


-- | If the output exceeds 100 exported output values, a string that identifies the next page of exports. If there is no additional page, this value is null.
lersNextToken :: Lens' ListExportsResponse (Maybe Text)
lersNextToken = lens _lersNextToken (\ s a -> s{_lersNextToken = a})

-- | The output for the 'ListExports' action.
lersExports :: Lens' ListExportsResponse [Export]
lersExports = lens _lersExports (\ s a -> s{_lersExports = a}) . _Default . _Coerce

-- | -- | The response status code.
lersResponseStatus :: Lens' ListExportsResponse Int
lersResponseStatus = lens _lersResponseStatus (\ s a -> s{_lersResponseStatus = a})

instance NFData ListExportsResponse where
