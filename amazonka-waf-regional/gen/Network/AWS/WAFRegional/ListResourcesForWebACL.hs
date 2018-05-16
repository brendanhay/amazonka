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
-- Module      : Network.AWS.WAFRegional.ListResourcesForWebACL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of resources associated with the specified web ACL.
--
--
module Network.AWS.WAFRegional.ListResourcesForWebACL
    (
    -- * Creating a Request
      listResourcesForWebACL
    , ListResourcesForWebACL
    -- * Request Lenses
    , lrfwaWebACLId

    -- * Destructuring the Response
    , listResourcesForWebACLResponse
    , ListResourcesForWebACLResponse
    -- * Response Lenses
    , lrfwarsResourceARNs
    , lrfwarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'listResourcesForWebACL' smart constructor.
newtype ListResourcesForWebACL = ListResourcesForWebACL'
  { _lrfwaWebACLId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourcesForWebACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrfwaWebACLId' - The unique identifier (ID) of the web ACL for which to list the associated resources.
listResourcesForWebACL
    :: Text -- ^ 'lrfwaWebACLId'
    -> ListResourcesForWebACL
listResourcesForWebACL pWebACLId_ =
  ListResourcesForWebACL' {_lrfwaWebACLId = pWebACLId_}


-- | The unique identifier (ID) of the web ACL for which to list the associated resources.
lrfwaWebACLId :: Lens' ListResourcesForWebACL Text
lrfwaWebACLId = lens _lrfwaWebACLId (\ s a -> s{_lrfwaWebACLId = a})

instance AWSRequest ListResourcesForWebACL where
        type Rs ListResourcesForWebACL =
             ListResourcesForWebACLResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 ListResourcesForWebACLResponse' <$>
                   (x .?> "ResourceArns" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ListResourcesForWebACL where

instance NFData ListResourcesForWebACL where

instance ToHeaders ListResourcesForWebACL where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.ListResourcesForWebACL" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListResourcesForWebACL where
        toJSON ListResourcesForWebACL'{..}
          = object
              (catMaybes [Just ("WebACLId" .= _lrfwaWebACLId)])

instance ToPath ListResourcesForWebACL where
        toPath = const "/"

instance ToQuery ListResourcesForWebACL where
        toQuery = const mempty

-- | /See:/ 'listResourcesForWebACLResponse' smart constructor.
data ListResourcesForWebACLResponse = ListResourcesForWebACLResponse'
  { _lrfwarsResourceARNs   :: !(Maybe [Text])
  , _lrfwarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourcesForWebACLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrfwarsResourceARNs' - An array of ARNs (Amazon Resource Names) of the resources associated with the specified web ACL. An array with zero elements is returned if there are no resources associated with the web ACL.
--
-- * 'lrfwarsResponseStatus' - -- | The response status code.
listResourcesForWebACLResponse
    :: Int -- ^ 'lrfwarsResponseStatus'
    -> ListResourcesForWebACLResponse
listResourcesForWebACLResponse pResponseStatus_ =
  ListResourcesForWebACLResponse'
    {_lrfwarsResourceARNs = Nothing, _lrfwarsResponseStatus = pResponseStatus_}


-- | An array of ARNs (Amazon Resource Names) of the resources associated with the specified web ACL. An array with zero elements is returned if there are no resources associated with the web ACL.
lrfwarsResourceARNs :: Lens' ListResourcesForWebACLResponse [Text]
lrfwarsResourceARNs = lens _lrfwarsResourceARNs (\ s a -> s{_lrfwarsResourceARNs = a}) . _Default . _Coerce

-- | -- | The response status code.
lrfwarsResponseStatus :: Lens' ListResourcesForWebACLResponse Int
lrfwarsResponseStatus = lens _lrfwarsResponseStatus (\ s a -> s{_lrfwarsResponseStatus = a})

instance NFData ListResourcesForWebACLResponse where
