-- Module      : Network.AWS.CloudSearchDomains
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

import Network.AWS.CloudSearchDomains.Search as Export
import Network.AWS.CloudSearchDomains.Suggest as Export
import Network.AWS.CloudSearchDomains.Types as Export
import Network.AWS.CloudSearchDomains.UploadDocuments as Export
import Network.AWS.CloudSearchDomains.Waiters as Export
