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
-- Module      : Amazonka.EC2.Types.ClassicLinkDnsSupport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClassicLinkDnsSupport where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the ClassicLink DNS support status of a VPC.
--
-- /See:/ 'newClassicLinkDnsSupport' smart constructor.
data ClassicLinkDnsSupport = ClassicLinkDnsSupport'
  { -- | Indicates whether ClassicLink DNS support is enabled for the VPC.
    classicLinkDnsSupported :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClassicLinkDnsSupport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classicLinkDnsSupported', 'classicLinkDnsSupport_classicLinkDnsSupported' - Indicates whether ClassicLink DNS support is enabled for the VPC.
--
-- 'vpcId', 'classicLinkDnsSupport_vpcId' - The ID of the VPC.
newClassicLinkDnsSupport ::
  ClassicLinkDnsSupport
newClassicLinkDnsSupport =
  ClassicLinkDnsSupport'
    { classicLinkDnsSupported =
        Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | Indicates whether ClassicLink DNS support is enabled for the VPC.
classicLinkDnsSupport_classicLinkDnsSupported :: Lens.Lens' ClassicLinkDnsSupport (Prelude.Maybe Prelude.Bool)
classicLinkDnsSupport_classicLinkDnsSupported = Lens.lens (\ClassicLinkDnsSupport' {classicLinkDnsSupported} -> classicLinkDnsSupported) (\s@ClassicLinkDnsSupport' {} a -> s {classicLinkDnsSupported = a} :: ClassicLinkDnsSupport)

-- | The ID of the VPC.
classicLinkDnsSupport_vpcId :: Lens.Lens' ClassicLinkDnsSupport (Prelude.Maybe Prelude.Text)
classicLinkDnsSupport_vpcId = Lens.lens (\ClassicLinkDnsSupport' {vpcId} -> vpcId) (\s@ClassicLinkDnsSupport' {} a -> s {vpcId = a} :: ClassicLinkDnsSupport)

instance Data.FromXML ClassicLinkDnsSupport where
  parseXML x =
    ClassicLinkDnsSupport'
      Prelude.<$> (x Data..@? "classicLinkDnsSupported")
      Prelude.<*> (x Data..@? "vpcId")

instance Prelude.Hashable ClassicLinkDnsSupport where
  hashWithSalt _salt ClassicLinkDnsSupport' {..} =
    _salt
      `Prelude.hashWithSalt` classicLinkDnsSupported
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData ClassicLinkDnsSupport where
  rnf ClassicLinkDnsSupport' {..} =
    Prelude.rnf classicLinkDnsSupported
      `Prelude.seq` Prelude.rnf vpcId
