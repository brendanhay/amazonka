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
-- Module      : Network.AWS.Route53Domains.UpdateTagsForDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation adds or updates tags for a specified domain.
--
--
-- All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.
--
module Network.AWS.Route53Domains.UpdateTagsForDomain
    (
    -- * Creating a Request
      updateTagsForDomain
    , UpdateTagsForDomain
    -- * Request Lenses
    , utfdTagsToUpdate
    , utfdDomainName

    -- * Destructuring the Response
    , updateTagsForDomainResponse
    , UpdateTagsForDomainResponse
    -- * Response Lenses
    , utfdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | The UpdateTagsForDomainRequest includes the following elements.
--
--
--
-- /See:/ 'updateTagsForDomain' smart constructor.
data UpdateTagsForDomain = UpdateTagsForDomain'
  { _utfdTagsToUpdate :: !(Maybe [Tag])
  , _utfdDomainName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTagsForDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utfdTagsToUpdate' - A list of the tag keys and values that you want to add or update. If you specify a key that already exists, the corresponding value will be replaced.
--
-- * 'utfdDomainName' - The domain for which you want to add or update tags.
updateTagsForDomain
    :: Text -- ^ 'utfdDomainName'
    -> UpdateTagsForDomain
updateTagsForDomain pDomainName_ =
  UpdateTagsForDomain'
    {_utfdTagsToUpdate = Nothing, _utfdDomainName = pDomainName_}


-- | A list of the tag keys and values that you want to add or update. If you specify a key that already exists, the corresponding value will be replaced.
utfdTagsToUpdate :: Lens' UpdateTagsForDomain [Tag]
utfdTagsToUpdate = lens _utfdTagsToUpdate (\ s a -> s{_utfdTagsToUpdate = a}) . _Default . _Coerce

-- | The domain for which you want to add or update tags.
utfdDomainName :: Lens' UpdateTagsForDomain Text
utfdDomainName = lens _utfdDomainName (\ s a -> s{_utfdDomainName = a})

instance AWSRequest UpdateTagsForDomain where
        type Rs UpdateTagsForDomain =
             UpdateTagsForDomainResponse
        request = postJSON route53Domains
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateTagsForDomainResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateTagsForDomain where

instance NFData UpdateTagsForDomain where

instance ToHeaders UpdateTagsForDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.UpdateTagsForDomain" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateTagsForDomain where
        toJSON UpdateTagsForDomain'{..}
          = object
              (catMaybes
                 [("TagsToUpdate" .=) <$> _utfdTagsToUpdate,
                  Just ("DomainName" .= _utfdDomainName)])

instance ToPath UpdateTagsForDomain where
        toPath = const "/"

instance ToQuery UpdateTagsForDomain where
        toQuery = const mempty

-- | /See:/ 'updateTagsForDomainResponse' smart constructor.
newtype UpdateTagsForDomainResponse = UpdateTagsForDomainResponse'
  { _utfdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTagsForDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utfdrsResponseStatus' - -- | The response status code.
updateTagsForDomainResponse
    :: Int -- ^ 'utfdrsResponseStatus'
    -> UpdateTagsForDomainResponse
updateTagsForDomainResponse pResponseStatus_ =
  UpdateTagsForDomainResponse' {_utfdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
utfdrsResponseStatus :: Lens' UpdateTagsForDomainResponse Int
utfdrsResponseStatus = lens _utfdrsResponseStatus (\ s a -> s{_utfdrsResponseStatus = a})

instance NFData UpdateTagsForDomainResponse where
