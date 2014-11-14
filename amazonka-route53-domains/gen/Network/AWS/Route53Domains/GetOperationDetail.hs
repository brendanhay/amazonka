{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.Route53Domains.GetOperationDetail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns the current status of an operation that is not
-- completed.
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
    , godrDomainName
    , godrMessage
    , godrOperationId
    , godrStatus
    , godrSubmittedDate
    , godrType
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Route53Domains.Types
import qualified GHC.Exts

newtype GetOperationDetail = GetOperationDetail
    { _godOperationId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetOperationDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'godOperationId' @::@ 'Text'
--
getOperationDetail :: Text -- ^ 'godOperationId'
                   -> GetOperationDetail
getOperationDetail p1 = GetOperationDetail
    { _godOperationId = p1
    }

-- | The identifier for the operation for which you want to get the status.
-- Amazon Route 53 returned the identifier in the response to the original
-- request. Type: String Default: None Required: Yes.
godOperationId :: Lens' GetOperationDetail Text
godOperationId = lens _godOperationId (\s a -> s { _godOperationId = a })

instance ToPath GetOperationDetail where
    toPath = const "/"

instance ToQuery GetOperationDetail where
    toQuery = const mempty

instance ToHeaders GetOperationDetail

instance ToBody GetOperationDetail where
    toBody = toBody . encode . _godOperationId

data GetOperationDetailResponse = GetOperationDetailResponse
    { _godrDomainName    :: Maybe Text
    , _godrMessage       :: Maybe Text
    , _godrOperationId   :: Maybe Text
    , _godrStatus        :: Maybe Text
    , _godrSubmittedDate :: Maybe RFC822
    , _godrType          :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetOperationDetailResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'godrDomainName' @::@ 'Maybe' 'Text'
--
-- * 'godrMessage' @::@ 'Maybe' 'Text'
--
-- * 'godrOperationId' @::@ 'Maybe' 'Text'
--
-- * 'godrStatus' @::@ 'Maybe' 'Text'
--
-- * 'godrSubmittedDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'godrType' @::@ 'Maybe' 'Text'
--
getOperationDetailResponse :: GetOperationDetailResponse
getOperationDetailResponse = GetOperationDetailResponse
    { _godrOperationId   = Nothing
    , _godrStatus        = Nothing
    , _godrMessage       = Nothing
    , _godrDomainName    = Nothing
    , _godrType          = Nothing
    , _godrSubmittedDate = Nothing
    }

-- | The name of a domain. Type: String.
godrDomainName :: Lens' GetOperationDetailResponse (Maybe Text)
godrDomainName = lens _godrDomainName (\s a -> s { _godrDomainName = a })

-- | Detailed information on the status including possible errors. Type:
-- String.
godrMessage :: Lens' GetOperationDetailResponse (Maybe Text)
godrMessage = lens _godrMessage (\s a -> s { _godrMessage = a })

-- | The identifier for the operation. Type: String.
godrOperationId :: Lens' GetOperationDetailResponse (Maybe Text)
godrOperationId = lens _godrOperationId (\s a -> s { _godrOperationId = a })

-- | The current status of the requested operation in the system. Type:
-- String.
godrStatus :: Lens' GetOperationDetailResponse (Maybe Text)
godrStatus = lens _godrStatus (\s a -> s { _godrStatus = a })

-- | The date when the request was submitted.
godrSubmittedDate :: Lens' GetOperationDetailResponse (Maybe UTCTime)
godrSubmittedDate =
    lens _godrSubmittedDate (\s a -> s { _godrSubmittedDate = a })
        . mapping _Time

-- | The type of operation that was requested. Type: String.
godrType :: Lens' GetOperationDetailResponse (Maybe Text)
godrType = lens _godrType (\s a -> s { _godrType = a })

instance AWSRequest GetOperationDetail where
    type Sv GetOperationDetail = Route53Domains
    type Rs GetOperationDetail = GetOperationDetailResponse

    request  = post
    response = jsonResponse $ \h o -> GetOperationDetailResponse
        <$> o .: "DomainName"
        <*> o .: "Message"
        <*> o .: "OperationId"
        <*> o .: "Status"
        <*> o .: "SubmittedDate"
        <*> o .: "Type"
