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
-- Module      : Network.AWS.DirectoryService.UpdateTrust
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the trust that has been set up between your AWS Managed Microsoft AD directory and an on-premises Active Directory.
--
--
module Network.AWS.DirectoryService.UpdateTrust
    (
    -- * Creating a Request
      updateTrust
    , UpdateTrust
    -- * Request Lenses
    , utSelectiveAuth
    , utTrustId

    -- * Destructuring the Response
    , updateTrustResponse
    , UpdateTrustResponse
    -- * Response Lenses
    , utrsRequestId
    , utrsTrustId
    , utrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateTrust' smart constructor.
data UpdateTrust = UpdateTrust'
  { _utSelectiveAuth :: !(Maybe SelectiveAuth)
  , _utTrustId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTrust' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utSelectiveAuth' - Updates selective authentication for the trust.
--
-- * 'utTrustId' - Identifier of the trust relationship.
updateTrust
    :: Text -- ^ 'utTrustId'
    -> UpdateTrust
updateTrust pTrustId_ =
  UpdateTrust' {_utSelectiveAuth = Nothing, _utTrustId = pTrustId_}


-- | Updates selective authentication for the trust.
utSelectiveAuth :: Lens' UpdateTrust (Maybe SelectiveAuth)
utSelectiveAuth = lens _utSelectiveAuth (\ s a -> s{_utSelectiveAuth = a})

-- | Identifier of the trust relationship.
utTrustId :: Lens' UpdateTrust Text
utTrustId = lens _utTrustId (\ s a -> s{_utTrustId = a})

instance AWSRequest UpdateTrust where
        type Rs UpdateTrust = UpdateTrustResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 UpdateTrustResponse' <$>
                   (x .?> "RequestId") <*> (x .?> "TrustId") <*>
                     (pure (fromEnum s)))

instance Hashable UpdateTrust where

instance NFData UpdateTrust where

instance ToHeaders UpdateTrust where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.UpdateTrust" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateTrust where
        toJSON UpdateTrust'{..}
          = object
              (catMaybes
                 [("SelectiveAuth" .=) <$> _utSelectiveAuth,
                  Just ("TrustId" .= _utTrustId)])

instance ToPath UpdateTrust where
        toPath = const "/"

instance ToQuery UpdateTrust where
        toQuery = const mempty

-- | /See:/ 'updateTrustResponse' smart constructor.
data UpdateTrustResponse = UpdateTrustResponse'
  { _utrsRequestId      :: !(Maybe Text)
  , _utrsTrustId        :: !(Maybe Text)
  , _utrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTrustResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utrsRequestId' - Undocumented member.
--
-- * 'utrsTrustId' - Identifier of the trust relationship.
--
-- * 'utrsResponseStatus' - -- | The response status code.
updateTrustResponse
    :: Int -- ^ 'utrsResponseStatus'
    -> UpdateTrustResponse
updateTrustResponse pResponseStatus_ =
  UpdateTrustResponse'
    { _utrsRequestId = Nothing
    , _utrsTrustId = Nothing
    , _utrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
utrsRequestId :: Lens' UpdateTrustResponse (Maybe Text)
utrsRequestId = lens _utrsRequestId (\ s a -> s{_utrsRequestId = a})

-- | Identifier of the trust relationship.
utrsTrustId :: Lens' UpdateTrustResponse (Maybe Text)
utrsTrustId = lens _utrsTrustId (\ s a -> s{_utrsTrustId = a})

-- | -- | The response status code.
utrsResponseStatus :: Lens' UpdateTrustResponse Int
utrsResponseStatus = lens _utrsResponseStatus (\ s a -> s{_utrsResponseStatus = a})

instance NFData UpdateTrustResponse where
