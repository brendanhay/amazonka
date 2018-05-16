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
-- Module      : Network.AWS.Route53Domains.GetOperationDetail
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the current status of an operation that is not completed.
--
--
module Network.AWS.Route53Domains.GetOperationDetail
    (
    -- * Creating a Request
      getOperationDetail
    , GetOperationDetail
    -- * Request Lenses
    , godOperationId

    -- * Destructuring the Response
    , getOperationDetailResponse
    , GetOperationDetailResponse
    -- * Response Lenses
    , godrsStatus
    , godrsSubmittedDate
    , godrsDomainName
    , godrsOperationId
    , godrsType
    , godrsMessage
    , godrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | The 'GetOperationDetail' request includes the following element.
--
--
--
-- /See:/ 'getOperationDetail' smart constructor.
newtype GetOperationDetail = GetOperationDetail'
  { _godOperationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetOperationDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'godOperationId' - The identifier for the operation for which you want to get the status. Amazon Route 53 returned the identifier in the response to the original request.
getOperationDetail
    :: Text -- ^ 'godOperationId'
    -> GetOperationDetail
getOperationDetail pOperationId_ =
  GetOperationDetail' {_godOperationId = pOperationId_}


-- | The identifier for the operation for which you want to get the status. Amazon Route 53 returned the identifier in the response to the original request.
godOperationId :: Lens' GetOperationDetail Text
godOperationId = lens _godOperationId (\ s a -> s{_godOperationId = a})

instance AWSRequest GetOperationDetail where
        type Rs GetOperationDetail =
             GetOperationDetailResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 GetOperationDetailResponse' <$>
                   (x .?> "Status") <*> (x .?> "SubmittedDate") <*>
                     (x .?> "DomainName")
                     <*> (x .?> "OperationId")
                     <*> (x .?> "Type")
                     <*> (x .?> "Message")
                     <*> (pure (fromEnum s)))

instance Hashable GetOperationDetail where

instance NFData GetOperationDetail where

instance ToHeaders GetOperationDetail where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.GetOperationDetail" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetOperationDetail where
        toJSON GetOperationDetail'{..}
          = object
              (catMaybes [Just ("OperationId" .= _godOperationId)])

instance ToPath GetOperationDetail where
        toPath = const "/"

instance ToQuery GetOperationDetail where
        toQuery = const mempty

-- | The GetOperationDetail response includes the following elements.
--
--
--
-- /See:/ 'getOperationDetailResponse' smart constructor.
data GetOperationDetailResponse = GetOperationDetailResponse'
  { _godrsStatus         :: !(Maybe OperationStatus)
  , _godrsSubmittedDate  :: !(Maybe POSIX)
  , _godrsDomainName     :: !(Maybe Text)
  , _godrsOperationId    :: !(Maybe Text)
  , _godrsType           :: !(Maybe OperationType)
  , _godrsMessage        :: !(Maybe Text)
  , _godrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetOperationDetailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'godrsStatus' - The current status of the requested operation in the system.
--
-- * 'godrsSubmittedDate' - The date when the request was submitted.
--
-- * 'godrsDomainName' - The name of a domain.
--
-- * 'godrsOperationId' - The identifier for the operation.
--
-- * 'godrsType' - The type of operation that was requested.
--
-- * 'godrsMessage' - Detailed information on the status including possible errors.
--
-- * 'godrsResponseStatus' - -- | The response status code.
getOperationDetailResponse
    :: Int -- ^ 'godrsResponseStatus'
    -> GetOperationDetailResponse
getOperationDetailResponse pResponseStatus_ =
  GetOperationDetailResponse'
    { _godrsStatus = Nothing
    , _godrsSubmittedDate = Nothing
    , _godrsDomainName = Nothing
    , _godrsOperationId = Nothing
    , _godrsType = Nothing
    , _godrsMessage = Nothing
    , _godrsResponseStatus = pResponseStatus_
    }


-- | The current status of the requested operation in the system.
godrsStatus :: Lens' GetOperationDetailResponse (Maybe OperationStatus)
godrsStatus = lens _godrsStatus (\ s a -> s{_godrsStatus = a})

-- | The date when the request was submitted.
godrsSubmittedDate :: Lens' GetOperationDetailResponse (Maybe UTCTime)
godrsSubmittedDate = lens _godrsSubmittedDate (\ s a -> s{_godrsSubmittedDate = a}) . mapping _Time

-- | The name of a domain.
godrsDomainName :: Lens' GetOperationDetailResponse (Maybe Text)
godrsDomainName = lens _godrsDomainName (\ s a -> s{_godrsDomainName = a})

-- | The identifier for the operation.
godrsOperationId :: Lens' GetOperationDetailResponse (Maybe Text)
godrsOperationId = lens _godrsOperationId (\ s a -> s{_godrsOperationId = a})

-- | The type of operation that was requested.
godrsType :: Lens' GetOperationDetailResponse (Maybe OperationType)
godrsType = lens _godrsType (\ s a -> s{_godrsType = a})

-- | Detailed information on the status including possible errors.
godrsMessage :: Lens' GetOperationDetailResponse (Maybe Text)
godrsMessage = lens _godrsMessage (\ s a -> s{_godrsMessage = a})

-- | -- | The response status code.
godrsResponseStatus :: Lens' GetOperationDetailResponse Int
godrsResponseStatus = lens _godrsResponseStatus (\ s a -> s{_godrsResponseStatus = a})

instance NFData GetOperationDetailResponse where
