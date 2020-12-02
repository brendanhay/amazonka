{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus where

import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.ElasticSearch.Types.VPCDerivedInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Status of the VPC options for the specified Elasticsearch domain.
--
--
--
-- /See:/ 'vpcDerivedInfoStatus' smart constructor.
data VPCDerivedInfoStatus = VPCDerivedInfoStatus'
  { _vdisOptions ::
      !VPCDerivedInfo,
    _vdisStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCDerivedInfoStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdisOptions' - Specifies the VPC options for the specified Elasticsearch domain.
--
-- * 'vdisStatus' - Specifies the status of the VPC options for the specified Elasticsearch domain.
vpcDerivedInfoStatus ::
  -- | 'vdisOptions'
  VPCDerivedInfo ->
  -- | 'vdisStatus'
  OptionStatus ->
  VPCDerivedInfoStatus
vpcDerivedInfoStatus pOptions_ pStatus_ =
  VPCDerivedInfoStatus'
    { _vdisOptions = pOptions_,
      _vdisStatus = pStatus_
    }

-- | Specifies the VPC options for the specified Elasticsearch domain.
vdisOptions :: Lens' VPCDerivedInfoStatus VPCDerivedInfo
vdisOptions = lens _vdisOptions (\s a -> s {_vdisOptions = a})

-- | Specifies the status of the VPC options for the specified Elasticsearch domain.
vdisStatus :: Lens' VPCDerivedInfoStatus OptionStatus
vdisStatus = lens _vdisStatus (\s a -> s {_vdisStatus = a})

instance FromJSON VPCDerivedInfoStatus where
  parseJSON =
    withObject
      "VPCDerivedInfoStatus"
      ( \x ->
          VPCDerivedInfoStatus' <$> (x .: "Options") <*> (x .: "Status")
      )

instance Hashable VPCDerivedInfoStatus

instance NFData VPCDerivedInfoStatus
