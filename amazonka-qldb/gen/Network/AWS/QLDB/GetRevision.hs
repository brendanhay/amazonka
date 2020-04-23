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
-- Module      : Network.AWS.QLDB.GetRevision
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.QLDB.GetRevision
    (
    -- * Creating a Request
      getRevision
    , GetRevision
    -- * Request Lenses
    , grDigestTipAddress
    , grName
    , grBlockAddress
    , grDocumentId

    -- * Destructuring the Response
    , getRevisionResponse
    , GetRevisionResponse
    -- * Response Lenses
    , grrsProof
    , grrsResponseStatus
    , grrsRevision
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.QLDB.Types
import Network.AWS.QLDB.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRevision' smart constructor.
data GetRevision = GetRevision'
  { _grDigestTipAddress :: !(Maybe (Sensitive ValueHolder))
  , _grName             :: !Text
  , _grBlockAddress     :: !(Sensitive ValueHolder)
  , _grDocumentId       :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRevision' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grDigestTipAddress' - Undocumented member.
--
-- * 'grName' - Undocumented member.
--
-- * 'grBlockAddress' - Undocumented member.
--
-- * 'grDocumentId' - Undocumented member.
getRevision
    :: Text -- ^ 'grName'
    -> ValueHolder -- ^ 'grBlockAddress'
    -> Text -- ^ 'grDocumentId'
    -> GetRevision
getRevision pName_ pBlockAddress_ pDocumentId_ =
  GetRevision'
    { _grDigestTipAddress = Nothing
    , _grName = pName_
    , _grBlockAddress = _Sensitive # pBlockAddress_
    , _grDocumentId = pDocumentId_
    }


-- | Undocumented member.
grDigestTipAddress :: Lens' GetRevision (Maybe ValueHolder)
grDigestTipAddress = lens _grDigestTipAddress (\ s a -> s{_grDigestTipAddress = a}) . mapping _Sensitive

-- | Undocumented member.
grName :: Lens' GetRevision Text
grName = lens _grName (\ s a -> s{_grName = a})

-- | Undocumented member.
grBlockAddress :: Lens' GetRevision ValueHolder
grBlockAddress = lens _grBlockAddress (\ s a -> s{_grBlockAddress = a}) . _Sensitive

-- | Undocumented member.
grDocumentId :: Lens' GetRevision Text
grDocumentId = lens _grDocumentId (\ s a -> s{_grDocumentId = a})

instance AWSRequest GetRevision where
        type Rs GetRevision = GetRevisionResponse
        request = postJSON qldb
        response
          = receiveJSON
              (\ s h x ->
                 GetRevisionResponse' <$>
                   (x .?> "Proof") <*> (pure (fromEnum s)) <*>
                     (x .:> "Revision"))

instance Hashable GetRevision where

instance NFData GetRevision where

instance ToHeaders GetRevision where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON GetRevision where
        toJSON GetRevision'{..}
          = object
              (catMaybes
                 [("DigestTipAddress" .=) <$> _grDigestTipAddress,
                  Just ("BlockAddress" .= _grBlockAddress),
                  Just ("DocumentId" .= _grDocumentId)])

instance ToPath GetRevision where
        toPath GetRevision'{..}
          = mconcat ["/ledgers/", toBS _grName, "/revision"]

instance ToQuery GetRevision where
        toQuery = const mempty

-- | /See:/ 'getRevisionResponse' smart constructor.
data GetRevisionResponse = GetRevisionResponse'
  { _grrsProof          :: !(Maybe (Sensitive ValueHolder))
  , _grrsResponseStatus :: !Int
  , _grrsRevision       :: !(Sensitive ValueHolder)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRevisionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsProof' - Undocumented member.
--
-- * 'grrsResponseStatus' - -- | The response status code.
--
-- * 'grrsRevision' - Undocumented member.
getRevisionResponse
    :: Int -- ^ 'grrsResponseStatus'
    -> ValueHolder -- ^ 'grrsRevision'
    -> GetRevisionResponse
getRevisionResponse pResponseStatus_ pRevision_ =
  GetRevisionResponse'
    { _grrsProof = Nothing
    , _grrsResponseStatus = pResponseStatus_
    , _grrsRevision = _Sensitive # pRevision_
    }


-- | Undocumented member.
grrsProof :: Lens' GetRevisionResponse (Maybe ValueHolder)
grrsProof = lens _grrsProof (\ s a -> s{_grrsProof = a}) . mapping _Sensitive

-- | -- | The response status code.
grrsResponseStatus :: Lens' GetRevisionResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\ s a -> s{_grrsResponseStatus = a})

-- | Undocumented member.
grrsRevision :: Lens' GetRevisionResponse ValueHolder
grrsRevision = lens _grrsRevision (\ s a -> s{_grrsRevision = a}) . _Sensitive

instance NFData GetRevisionResponse where
