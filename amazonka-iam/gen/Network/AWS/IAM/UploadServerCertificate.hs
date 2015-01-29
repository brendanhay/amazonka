{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UploadServerCertificate
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

-- | Uploads a server certificate entity for the AWS account. The server
-- certificate entity includes a public key certificate, a private key, and an
-- optional certificate chain, which should all be PEM-encoded.
--
-- For information about the number of server certificates you can upload, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /Using IAM/ guide.
--
-- Because the body of the public key certificate, private key, and the
-- certificate chain can be large, you should use POST rather than GET when
-- calling 'UploadServerCertificate'. For information about setting up signatures
-- and authorization through the API, go to <http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html Signing AWS API Requests> in the /AWSGeneral Reference/. For general information about using the Query API with
-- IAM, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UploadServerCertificate.html>
module Network.AWS.IAM.UploadServerCertificate
    (
    -- * Request
      UploadServerCertificate
    -- ** Request constructor
    , uploadServerCertificate
    -- ** Request lenses
    , uscCertificateBody
    , uscCertificateChain
    , uscPath
    , uscPrivateKey
    , uscServerCertificateName

    -- * Response
    , UploadServerCertificateResponse
    -- ** Response constructor
    , uploadServerCertificateResponse
    -- ** Response lenses
    , uscrServerCertificateMetadata
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data UploadServerCertificate = UploadServerCertificate
    { _uscCertificateBody       :: Text
    , _uscCertificateChain      :: Maybe Text
    , _uscPath                  :: Maybe Text
    , _uscPrivateKey            :: Sensitive Text
    , _uscServerCertificateName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UploadServerCertificate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uscCertificateBody' @::@ 'Text'
--
-- * 'uscCertificateChain' @::@ 'Maybe' 'Text'
--
-- * 'uscPath' @::@ 'Maybe' 'Text'
--
-- * 'uscPrivateKey' @::@ 'Text'
--
-- * 'uscServerCertificateName' @::@ 'Text'
--
uploadServerCertificate :: Text -- ^ 'uscServerCertificateName'
                        -> Text -- ^ 'uscCertificateBody'
                        -> Text -- ^ 'uscPrivateKey'
                        -> UploadServerCertificate
uploadServerCertificate p1 p2 p3 = UploadServerCertificate
    { _uscServerCertificateName = p1
    , _uscCertificateBody       = p2
    , _uscPrivateKey            = withIso _Sensitive (const id) p3
    , _uscPath                  = Nothing
    , _uscCertificateChain      = Nothing
    }

-- | The contents of the public key certificate in PEM-encoded format.
uscCertificateBody :: Lens' UploadServerCertificate Text
uscCertificateBody =
    lens _uscCertificateBody (\s a -> s { _uscCertificateBody = a })

-- | The contents of the certificate chain. This is typically a concatenation of
-- the PEM-encoded public key certificates of the chain.
uscCertificateChain :: Lens' UploadServerCertificate (Maybe Text)
uscCertificateChain =
    lens _uscCertificateChain (\s a -> s { _uscCertificateChain = a })

-- | The path for the server certificate. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- This parameter is optional. If it is not included, it defaults to a slash
-- (/).
--
-- If you are uploading a server certificate specifically for use with Amazon
-- CloudFront distributions, you must specify a path using the '--path' option.
-- The path must begin with '/cloudfront' and must include a trailing slash (for
-- example, '/cloudfront/test/').
uscPath :: Lens' UploadServerCertificate (Maybe Text)
uscPath = lens _uscPath (\s a -> s { _uscPath = a })

-- | The contents of the private key in PEM-encoded format.
uscPrivateKey :: Lens' UploadServerCertificate Text
uscPrivateKey = lens _uscPrivateKey (\s a -> s { _uscPrivateKey = a }) . _Sensitive

-- | The name for the server certificate. Do not include the path in this value.
uscServerCertificateName :: Lens' UploadServerCertificate Text
uscServerCertificateName =
    lens _uscServerCertificateName
        (\s a -> s { _uscServerCertificateName = a })

newtype UploadServerCertificateResponse = UploadServerCertificateResponse
    { _uscrServerCertificateMetadata :: Maybe ServerCertificateMetadata
    } deriving (Eq, Read, Show)

-- | 'UploadServerCertificateResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uscrServerCertificateMetadata' @::@ 'Maybe' 'ServerCertificateMetadata'
--
uploadServerCertificateResponse :: UploadServerCertificateResponse
uploadServerCertificateResponse = UploadServerCertificateResponse
    { _uscrServerCertificateMetadata = Nothing
    }

-- | The meta information of the uploaded server certificate without its
-- certificate body, certificate chain, and private key.
uscrServerCertificateMetadata :: Lens' UploadServerCertificateResponse (Maybe ServerCertificateMetadata)
uscrServerCertificateMetadata =
    lens _uscrServerCertificateMetadata
        (\s a -> s { _uscrServerCertificateMetadata = a })

instance ToPath UploadServerCertificate where
    toPath = const "/"

instance ToQuery UploadServerCertificate where
    toQuery UploadServerCertificate{..} = mconcat
        [ "CertificateBody"       =? _uscCertificateBody
        , "CertificateChain"      =? _uscCertificateChain
        , "Path"                  =? _uscPath
        , "PrivateKey"            =? _uscPrivateKey
        , "ServerCertificateName" =? _uscServerCertificateName
        ]

instance ToHeaders UploadServerCertificate

instance AWSRequest UploadServerCertificate where
    type Sv UploadServerCertificate = IAM
    type Rs UploadServerCertificate = UploadServerCertificateResponse

    request  = post "UploadServerCertificate"
    response = xmlResponse

instance FromXML UploadServerCertificateResponse where
    parseXML = withElement "UploadServerCertificateResult" $ \x -> UploadServerCertificateResponse
        <$> x .@? "ServerCertificateMetadata"
