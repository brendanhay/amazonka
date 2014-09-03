{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DeleteHsmClientCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified HSM client certificate.
module Network.AWS.Redshift.V2012_12_01.DeleteHsmClientCertificate
    (
    -- * Request
      DeleteHsmClientCertificate
    -- ** Request constructor
    , deleteHsmClientCertificate
    -- ** Request lenses
    , dhccmHsmClientCertificateIdentifier

    -- * Response
    , DeleteHsmClientCertificateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteHsmClientCertificate' request.
deleteHsmClientCertificate :: Text -- ^ 'dhccmHsmClientCertificateIdentifier'
                           -> DeleteHsmClientCertificate
deleteHsmClientCertificate p1 = DeleteHsmClientCertificate
    { _dhccmHsmClientCertificateIdentifier = p1
    }

data DeleteHsmClientCertificate = DeleteHsmClientCertificate
    { _dhccmHsmClientCertificateIdentifier :: Text
      -- ^ The identifier of the HSM client certificate to be deleted.
    } deriving (Show, Generic)

-- | The identifier of the HSM client certificate to be deleted.
dhccmHsmClientCertificateIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteHsmClientCertificate
    -> f DeleteHsmClientCertificate
dhccmHsmClientCertificateIdentifier f x =
    (\y -> x { _dhccmHsmClientCertificateIdentifier = y })
       <$> f (_dhccmHsmClientCertificateIdentifier x)
{-# INLINE dhccmHsmClientCertificateIdentifier #-}

instance ToQuery DeleteHsmClientCertificate where
    toQuery = genericQuery def

data DeleteHsmClientCertificateResponse = DeleteHsmClientCertificateResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteHsmClientCertificate where
    type Sv DeleteHsmClientCertificate = Redshift
    type Rs DeleteHsmClientCertificate = DeleteHsmClientCertificateResponse

    request = post "DeleteHsmClientCertificate"
    response _ = nullaryResponse DeleteHsmClientCertificateResponse
