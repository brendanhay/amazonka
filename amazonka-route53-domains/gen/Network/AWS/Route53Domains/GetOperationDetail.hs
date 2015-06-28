{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53Domains.GetOperationDetail
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns the current status of an operation that is not
-- completed.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-GetOperationDetail.html>
module Network.AWS.Route53Domains.GetOperationDetail
    (
    -- * Request
      GetOperationDetail
    -- ** Request constructor
    , getOperationDetail
    -- ** Request lenses
    , godOperationId

    -- * Response
    , GetOperationDetailResponse
    -- ** Response constructor
    , getOperationDetailResponse
    -- ** Response lenses
    , godrSubmittedDate
    , godrDomainName
    , godrOperationId
    , godrType
    , godrMessage
    , godrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types

-- | The GetOperationDetail request includes the following element.
--
-- /See:/ 'getOperationDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'godOperationId'
newtype GetOperationDetail = GetOperationDetail'
    { _godOperationId :: Text
    } deriving (Eq,Read,Show)

-- | 'GetOperationDetail' smart constructor.
getOperationDetail :: Text -> GetOperationDetail
getOperationDetail pOperationId =
    GetOperationDetail'
    { _godOperationId = pOperationId
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
                     <*> (pure s))

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
-- * 'godrSubmittedDate'
--
-- * 'godrDomainName'
--
-- * 'godrOperationId'
--
-- * 'godrType'
--
-- * 'godrMessage'
--
-- * 'godrStatus'
data GetOperationDetailResponse = GetOperationDetailResponse'
    { _godrSubmittedDate :: !(Maybe POSIX)
    , _godrDomainName    :: !(Maybe Text)
    , _godrOperationId   :: !(Maybe Text)
    , _godrType          :: !(Maybe OperationType)
    , _godrMessage       :: !(Maybe Text)
    , _godrStatus        :: !Status
    } deriving (Eq,Show)

-- | 'GetOperationDetailResponse' smart constructor.
getOperationDetailResponse :: Status -> GetOperationDetailResponse
getOperationDetailResponse pStatus =
    GetOperationDetailResponse'
    { _godrSubmittedDate = Nothing
    , _godrDomainName = Nothing
    , _godrOperationId = Nothing
    , _godrType = Nothing
    , _godrMessage = Nothing
    , _godrStatus = pStatus
    }

-- | The date when the request was submitted.
godrSubmittedDate :: Lens' GetOperationDetailResponse (Maybe UTCTime)
godrSubmittedDate = lens _godrSubmittedDate (\ s a -> s{_godrSubmittedDate = a}) . mapping _Time;

-- | The name of a domain.
--
-- Type: String
godrDomainName :: Lens' GetOperationDetailResponse (Maybe Text)
godrDomainName = lens _godrDomainName (\ s a -> s{_godrDomainName = a});

-- | The identifier for the operation.
--
-- Type: String
godrOperationId :: Lens' GetOperationDetailResponse (Maybe Text)
godrOperationId = lens _godrOperationId (\ s a -> s{_godrOperationId = a});

-- | The type of operation that was requested.
--
-- Type: String
godrType :: Lens' GetOperationDetailResponse (Maybe OperationType)
godrType = lens _godrType (\ s a -> s{_godrType = a});

-- | Detailed information on the status including possible errors.
--
-- Type: String
godrMessage :: Lens' GetOperationDetailResponse (Maybe Text)
godrMessage = lens _godrMessage (\ s a -> s{_godrMessage = a});

-- | FIXME: Undocumented member.
godrStatus :: Lens' GetOperationDetailResponse Status
godrStatus = lens _godrStatus (\ s a -> s{_godrStatus = a});
