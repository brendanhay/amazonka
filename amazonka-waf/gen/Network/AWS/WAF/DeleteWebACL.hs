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
-- Module      : Network.AWS.WAF.DeleteWebACL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'WebACL' . You can't delete a @WebACL@ if it still contains any @Rules@ .
--
--
-- To delete a @WebACL@ , perform the following steps:
--
--     * Update the @WebACL@ to remove @Rules@ , if any. For more information, see 'UpdateWebACL' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteWebACL@ request.
--
--     * Submit a @DeleteWebACL@ request.
--
--
--
module Network.AWS.WAF.DeleteWebACL
    (
    -- * Creating a Request
      deleteWebACL
    , DeleteWebACL
    -- * Request Lenses
    , dwaWebACLId
    , dwaChangeToken

    -- * Destructuring the Response
    , deleteWebACLResponse
    , DeleteWebACLResponse
    -- * Response Lenses
    , dwarsChangeToken
    , dwarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'deleteWebACL' smart constructor.
data DeleteWebACL = DeleteWebACL'
  { _dwaWebACLId    :: !Text
  , _dwaChangeToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteWebACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwaWebACLId' - The @WebACLId@ of the 'WebACL' that you want to delete. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- * 'dwaChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
deleteWebACL
    :: Text -- ^ 'dwaWebACLId'
    -> Text -- ^ 'dwaChangeToken'
    -> DeleteWebACL
deleteWebACL pWebACLId_ pChangeToken_ =
  DeleteWebACL' {_dwaWebACLId = pWebACLId_, _dwaChangeToken = pChangeToken_}


-- | The @WebACLId@ of the 'WebACL' that you want to delete. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
dwaWebACLId :: Lens' DeleteWebACL Text
dwaWebACLId = lens _dwaWebACLId (\ s a -> s{_dwaWebACLId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
dwaChangeToken :: Lens' DeleteWebACL Text
dwaChangeToken = lens _dwaChangeToken (\ s a -> s{_dwaChangeToken = a})

instance AWSRequest DeleteWebACL where
        type Rs DeleteWebACL = DeleteWebACLResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 DeleteWebACLResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable DeleteWebACL where

instance NFData DeleteWebACL where

instance ToHeaders DeleteWebACL where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.DeleteWebACL" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteWebACL where
        toJSON DeleteWebACL'{..}
          = object
              (catMaybes
                 [Just ("WebACLId" .= _dwaWebACLId),
                  Just ("ChangeToken" .= _dwaChangeToken)])

instance ToPath DeleteWebACL where
        toPath = const "/"

instance ToQuery DeleteWebACL where
        toQuery = const mempty

-- | /See:/ 'deleteWebACLResponse' smart constructor.
data DeleteWebACLResponse = DeleteWebACLResponse'
  { _dwarsChangeToken    :: !(Maybe Text)
  , _dwarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteWebACLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwarsChangeToken' - The @ChangeToken@ that you used to submit the @DeleteWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'dwarsResponseStatus' - -- | The response status code.
deleteWebACLResponse
    :: Int -- ^ 'dwarsResponseStatus'
    -> DeleteWebACLResponse
deleteWebACLResponse pResponseStatus_ =
  DeleteWebACLResponse'
    {_dwarsChangeToken = Nothing, _dwarsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @DeleteWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
dwarsChangeToken :: Lens' DeleteWebACLResponse (Maybe Text)
dwarsChangeToken = lens _dwarsChangeToken (\ s a -> s{_dwarsChangeToken = a})

-- | -- | The response status code.
dwarsResponseStatus :: Lens' DeleteWebACLResponse Int
dwarsResponseStatus = lens _dwarsResponseStatus (\ s a -> s{_dwarsResponseStatus = a})

instance NFData DeleteWebACLResponse where
