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
-- Module      : Amazonka.Route53.Types.HostedZoneConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.HostedZoneConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

-- | A complex type that contains an optional comment about your hosted zone.
-- If you don\'t want to specify a comment, omit both the
-- @HostedZoneConfig@ and @Comment@ elements.
--
-- /See:/ 'newHostedZoneConfig' smart constructor.
data HostedZoneConfig = HostedZoneConfig'
  { -- | A value that indicates whether this is a private hosted zone.
    privateZone :: Prelude.Maybe Prelude.Bool,
    -- | Any comments that you want to include about the hosted zone.
    comment :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HostedZoneConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateZone', 'hostedZoneConfig_privateZone' - A value that indicates whether this is a private hosted zone.
--
-- 'comment', 'hostedZoneConfig_comment' - Any comments that you want to include about the hosted zone.
newHostedZoneConfig ::
  HostedZoneConfig
newHostedZoneConfig =
  HostedZoneConfig'
    { privateZone = Prelude.Nothing,
      comment = Prelude.Nothing
    }

-- | A value that indicates whether this is a private hosted zone.
hostedZoneConfig_privateZone :: Lens.Lens' HostedZoneConfig (Prelude.Maybe Prelude.Bool)
hostedZoneConfig_privateZone = Lens.lens (\HostedZoneConfig' {privateZone} -> privateZone) (\s@HostedZoneConfig' {} a -> s {privateZone = a} :: HostedZoneConfig)

-- | Any comments that you want to include about the hosted zone.
hostedZoneConfig_comment :: Lens.Lens' HostedZoneConfig (Prelude.Maybe Prelude.Text)
hostedZoneConfig_comment = Lens.lens (\HostedZoneConfig' {comment} -> comment) (\s@HostedZoneConfig' {} a -> s {comment = a} :: HostedZoneConfig)

instance Core.FromXML HostedZoneConfig where
  parseXML x =
    HostedZoneConfig'
      Prelude.<$> (x Core..@? "PrivateZone")
      Prelude.<*> (x Core..@? "Comment")

instance Prelude.Hashable HostedZoneConfig where
  hashWithSalt _salt HostedZoneConfig' {..} =
    _salt `Prelude.hashWithSalt` privateZone
      `Prelude.hashWithSalt` comment

instance Prelude.NFData HostedZoneConfig where
  rnf HostedZoneConfig' {..} =
    Prelude.rnf privateZone
      `Prelude.seq` Prelude.rnf comment

instance Core.ToXML HostedZoneConfig where
  toXML HostedZoneConfig' {..} =
    Prelude.mconcat
      [ "PrivateZone" Core.@= privateZone,
        "Comment" Core.@= comment
      ]
