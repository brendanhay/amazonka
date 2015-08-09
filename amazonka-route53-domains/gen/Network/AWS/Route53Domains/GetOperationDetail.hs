{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.GetOperationDetail
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the current status of an operation that is not
-- completed.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/api-GetOperationDetail.html AWS API Reference> for GetOperationDetail.
module Network.AWS.Route53Domains.GetOperationDetail
    (
    -- * Creating a Request
      GetOperationDetail
    , getOperationDetail
    -- * Request Lenses
    , godOperationId

    -- * Destructuring the Response
    , GetOperationDetailResponse
    , getOperationDetailResponse
    -- * Response Lenses
    , godrsSubmittedDate
    , godrsDomainName
    , godrsOperationId
    , godrsType
    , godrsMessage
    , godrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types
import           Network.AWS.Route53Domains.Types.Product

-- | The GetOperationDetail request includes the following element.
--
-- /See:/ 'getOperationDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'godOperationId'
newtype GetOperationDetail = GetOperationDetail'
    { _godOperationId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetOperationDetail' smart constructor.
getOperationDetail :: Text -> GetOperationDetail
getOperationDetail pOperationId_ =
    GetOperationDetail'
    { _godOperationId = pOperationId_
    }

-- | The identifier for the operation for which you want to get the status.
-- Amazon Route 53 returned the identifier in the response to the original
-- request.
--
-- Type: String
--
-- Default: None
--
-- Required: Yes
godOperationId :: Lens' GetOperationDetail Text
godOperationId = lens _godOperationId (\ s a -> s{_godOperationId = a});

instance AWSRequest GetOperationDetail where
        type Sv GetOperationDetail = Route53Domains
        type Rs GetOperationDetail =
             GetOperationDetailResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetOperationDetailResponse' <$>
                   (x .?> "SubmittedDate") <*> (x .?> "DomainName") <*>
                     (x .?> "OperationId")
                     <*> (x .?> "Type")
                     <*> (x .?> "Message")
                     <*> (pure (fromEnum s)))

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
          = object ["OperationId" .= _godOperationId]

instance ToPath GetOperationDetail where
        toPath = const "/"

instance ToQuery GetOperationDetail where
        toQuery = const mempty

-- | The GetOperationDetail response includes the following elements.
--
-- /See:/ 'getOperationDetailResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'godrsSubmittedDate'
--
-- * 'godrsDomainName'
--
-- * 'godrsOperationId'
--
-- * 'godrsType'
--
-- * 'godrsMessage'
--
-- * 'godrsStatus'
data GetOperationDetailResponse = GetOperationDetailResponse'
    { _godrsSubmittedDate :: !(Maybe POSIX)
    , _godrsDomainName    :: !(Maybe Text)
    , _godrsOperationId   :: !(Maybe Text)
    , _godrsType          :: !(Maybe OperationType)
    , _godrsMessage       :: !(Maybe Text)
    , _godrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetOperationDetailResponse' smart constructor.
getOperationDetailResponse :: Int -> GetOperationDetailResponse
getOperationDetailResponse pStatus_ =
    GetOperationDetailResponse'
    { _godrsSubmittedDate = Nothing
    , _godrsDomainName = Nothing
    , _godrsOperationId = Nothing
    , _godrsType = Nothing
    , _godrsMessage = Nothing
    , _godrsStatus = pStatus_
    }

-- | The date when the request was submitted.
godrsSubmittedDate :: Lens' GetOperationDetailResponse (Maybe UTCTime)
godrsSubmittedDate = lens _godrsSubmittedDate (\ s a -> s{_godrsSubmittedDate = a}) . mapping _Time;

-- | The name of a domain.
--
-- Type: String
godrsDomainName :: Lens' GetOperationDetailResponse (Maybe Text)
godrsDomainName = lens _godrsDomainName (\ s a -> s{_godrsDomainName = a});

-- | The identifier for the operation.
--
-- Type: String
godrsOperationId :: Lens' GetOperationDetailResponse (Maybe Text)
godrsOperationId = lens _godrsOperationId (\ s a -> s{_godrsOperationId = a});

-- | The type of operation that was requested.
--
-- Type: String
godrsType :: Lens' GetOperationDetailResponse (Maybe OperationType)
godrsType = lens _godrsType (\ s a -> s{_godrsType = a});

-- | Detailed information on the status including possible errors.
--
-- Type: String
godrsMessage :: Lens' GetOperationDetailResponse (Maybe Text)
godrsMessage = lens _godrsMessage (\ s a -> s{_godrsMessage = a});

-- | Undocumented member.
godrsStatus :: Lens' GetOperationDetailResponse Int
godrsStatus = lens _godrsStatus (\ s a -> s{_godrsStatus = a});
