{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AccessPoliciesStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AccessPoliciesStatus where

import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configured access rules for the domain's document and search endpoints, and the current status of those rules.
--
--
--
-- /See:/ 'accessPoliciesStatus' smart constructor.
data AccessPoliciesStatus = AccessPoliciesStatus'
  { _apsOptions ::
      !Text,
    _apsStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccessPoliciesStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apsOptions' - The access policy configured for the Elasticsearch domain. Access policies may be resource-based, IP-based, or IAM-based. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-access-policies Configuring Access Policies> for more information.
--
-- * 'apsStatus' - The status of the access policy for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
accessPoliciesStatus ::
  -- | 'apsOptions'
  Text ->
  -- | 'apsStatus'
  OptionStatus ->
  AccessPoliciesStatus
accessPoliciesStatus pOptions_ pStatus_ =
  AccessPoliciesStatus'
    { _apsOptions = pOptions_,
      _apsStatus = pStatus_
    }

-- | The access policy configured for the Elasticsearch domain. Access policies may be resource-based, IP-based, or IAM-based. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-access-policies Configuring Access Policies> for more information.
apsOptions :: Lens' AccessPoliciesStatus Text
apsOptions = lens _apsOptions (\s a -> s {_apsOptions = a})

-- | The status of the access policy for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
apsStatus :: Lens' AccessPoliciesStatus OptionStatus
apsStatus = lens _apsStatus (\s a -> s {_apsStatus = a})

instance FromJSON AccessPoliciesStatus where
  parseJSON =
    withObject
      "AccessPoliciesStatus"
      ( \x ->
          AccessPoliciesStatus' <$> (x .: "Options") <*> (x .: "Status")
      )

instance Hashable AccessPoliciesStatus

instance NFData AccessPoliciesStatus
