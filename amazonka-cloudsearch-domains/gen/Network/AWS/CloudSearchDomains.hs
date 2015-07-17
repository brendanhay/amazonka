{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | You use the AmazonCloudSearch2013 API to upload documents to a search
-- domain and search those documents.
--
-- The endpoints for submitting @UploadDocuments@, @Search@, and @Suggest@
-- requests are domain-specific. To get the endpoints for your domain, use
-- the Amazon CloudSearch configuration service @DescribeDomains@ action.
-- The domain endpoints are also displayed on the domain dashboard in the
-- Amazon CloudSearch console. You submit suggest requests to the search
-- endpoint.
--
-- For more information, see the
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide Amazon CloudSearch Developer Guide>.
module Network.AWS.CloudSearchDomains
    ( module Export
    ) where

import           Network.AWS.CloudSearchDomains.Search          as Export
import           Network.AWS.CloudSearchDomains.Suggest         as Export
import           Network.AWS.CloudSearchDomains.Types           as Export
import           Network.AWS.CloudSearchDomains.Types.Product   as Export
import           Network.AWS.CloudSearchDomains.Types.Sum       as Export
import           Network.AWS.CloudSearchDomains.UploadDocuments as Export
import           Network.AWS.CloudSearchDomains.Waiters         as Export
