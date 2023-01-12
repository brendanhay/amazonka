{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSDKMessaging.Types.ElasticChannelConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ElasticChannelConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The attributes required to configure and create an elastic channel. An
-- elastic channel can support a maximum of 1-million members.
--
-- /See:/ 'newElasticChannelConfiguration' smart constructor.
data ElasticChannelConfiguration = ElasticChannelConfiguration'
  { -- | The maximum number of SubChannels that you want to allow in the elastic
    -- channel.
    maximumSubChannels :: Prelude.Natural,
    -- | The maximum number of members allowed in a SubChannel.
    targetMembershipsPerSubChannel :: Prelude.Natural,
    -- | The minimum allowed percentage of TargetMembershipsPerSubChannel users.
    -- Ceil of the calculated value is used in balancing members among
    -- SubChannels of the elastic channel.
    minimumMembershipPercentage :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticChannelConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumSubChannels', 'elasticChannelConfiguration_maximumSubChannels' - The maximum number of SubChannels that you want to allow in the elastic
-- channel.
--
-- 'targetMembershipsPerSubChannel', 'elasticChannelConfiguration_targetMembershipsPerSubChannel' - The maximum number of members allowed in a SubChannel.
--
-- 'minimumMembershipPercentage', 'elasticChannelConfiguration_minimumMembershipPercentage' - The minimum allowed percentage of TargetMembershipsPerSubChannel users.
-- Ceil of the calculated value is used in balancing members among
-- SubChannels of the elastic channel.
newElasticChannelConfiguration ::
  -- | 'maximumSubChannels'
  Prelude.Natural ->
  -- | 'targetMembershipsPerSubChannel'
  Prelude.Natural ->
  -- | 'minimumMembershipPercentage'
  Prelude.Natural ->
  ElasticChannelConfiguration
newElasticChannelConfiguration
  pMaximumSubChannels_
  pTargetMembershipsPerSubChannel_
  pMinimumMembershipPercentage_ =
    ElasticChannelConfiguration'
      { maximumSubChannels =
          pMaximumSubChannels_,
        targetMembershipsPerSubChannel =
          pTargetMembershipsPerSubChannel_,
        minimumMembershipPercentage =
          pMinimumMembershipPercentage_
      }

-- | The maximum number of SubChannels that you want to allow in the elastic
-- channel.
elasticChannelConfiguration_maximumSubChannels :: Lens.Lens' ElasticChannelConfiguration Prelude.Natural
elasticChannelConfiguration_maximumSubChannels = Lens.lens (\ElasticChannelConfiguration' {maximumSubChannels} -> maximumSubChannels) (\s@ElasticChannelConfiguration' {} a -> s {maximumSubChannels = a} :: ElasticChannelConfiguration)

-- | The maximum number of members allowed in a SubChannel.
elasticChannelConfiguration_targetMembershipsPerSubChannel :: Lens.Lens' ElasticChannelConfiguration Prelude.Natural
elasticChannelConfiguration_targetMembershipsPerSubChannel = Lens.lens (\ElasticChannelConfiguration' {targetMembershipsPerSubChannel} -> targetMembershipsPerSubChannel) (\s@ElasticChannelConfiguration' {} a -> s {targetMembershipsPerSubChannel = a} :: ElasticChannelConfiguration)

-- | The minimum allowed percentage of TargetMembershipsPerSubChannel users.
-- Ceil of the calculated value is used in balancing members among
-- SubChannels of the elastic channel.
elasticChannelConfiguration_minimumMembershipPercentage :: Lens.Lens' ElasticChannelConfiguration Prelude.Natural
elasticChannelConfiguration_minimumMembershipPercentage = Lens.lens (\ElasticChannelConfiguration' {minimumMembershipPercentage} -> minimumMembershipPercentage) (\s@ElasticChannelConfiguration' {} a -> s {minimumMembershipPercentage = a} :: ElasticChannelConfiguration)

instance Data.FromJSON ElasticChannelConfiguration where
  parseJSON =
    Data.withObject
      "ElasticChannelConfiguration"
      ( \x ->
          ElasticChannelConfiguration'
            Prelude.<$> (x Data..: "MaximumSubChannels")
            Prelude.<*> (x Data..: "TargetMembershipsPerSubChannel")
            Prelude.<*> (x Data..: "MinimumMembershipPercentage")
      )

instance Prelude.Hashable ElasticChannelConfiguration where
  hashWithSalt _salt ElasticChannelConfiguration' {..} =
    _salt `Prelude.hashWithSalt` maximumSubChannels
      `Prelude.hashWithSalt` targetMembershipsPerSubChannel
      `Prelude.hashWithSalt` minimumMembershipPercentage

instance Prelude.NFData ElasticChannelConfiguration where
  rnf ElasticChannelConfiguration' {..} =
    Prelude.rnf maximumSubChannels
      `Prelude.seq` Prelude.rnf targetMembershipsPerSubChannel
      `Prelude.seq` Prelude.rnf minimumMembershipPercentage

instance Data.ToJSON ElasticChannelConfiguration where
  toJSON ElasticChannelConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MaximumSubChannels" Data..= maximumSubChannels),
            Prelude.Just
              ( "TargetMembershipsPerSubChannel"
                  Data..= targetMembershipsPerSubChannel
              ),
            Prelude.Just
              ( "MinimumMembershipPercentage"
                  Data..= minimumMembershipPercentage
              )
          ]
      )
