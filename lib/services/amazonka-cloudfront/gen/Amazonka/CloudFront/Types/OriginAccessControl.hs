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
-- Module      : Amazonka.CloudFront.Types.OriginAccessControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginAccessControl where

import Amazonka.CloudFront.Types.OriginAccessControlConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A CloudFront origin access control, including its unique identifier.
--
-- /See:/ 'newOriginAccessControl' smart constructor.
data OriginAccessControl = OriginAccessControl'
  { -- | The origin access control.
    originAccessControlConfig :: Prelude.Maybe OriginAccessControlConfig,
    -- | The unique identifier of the origin access control.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginAccessControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originAccessControlConfig', 'originAccessControl_originAccessControlConfig' - The origin access control.
--
-- 'id', 'originAccessControl_id' - The unique identifier of the origin access control.
newOriginAccessControl ::
  -- | 'id'
  Prelude.Text ->
  OriginAccessControl
newOriginAccessControl pId_ =
  OriginAccessControl'
    { originAccessControlConfig =
        Prelude.Nothing,
      id = pId_
    }

-- | The origin access control.
originAccessControl_originAccessControlConfig :: Lens.Lens' OriginAccessControl (Prelude.Maybe OriginAccessControlConfig)
originAccessControl_originAccessControlConfig = Lens.lens (\OriginAccessControl' {originAccessControlConfig} -> originAccessControlConfig) (\s@OriginAccessControl' {} a -> s {originAccessControlConfig = a} :: OriginAccessControl)

-- | The unique identifier of the origin access control.
originAccessControl_id :: Lens.Lens' OriginAccessControl Prelude.Text
originAccessControl_id = Lens.lens (\OriginAccessControl' {id} -> id) (\s@OriginAccessControl' {} a -> s {id = a} :: OriginAccessControl)

instance Data.FromXML OriginAccessControl where
  parseXML x =
    OriginAccessControl'
      Prelude.<$> (x Data..@? "OriginAccessControlConfig")
      Prelude.<*> (x Data..@ "Id")

instance Prelude.Hashable OriginAccessControl where
  hashWithSalt _salt OriginAccessControl' {..} =
    _salt
      `Prelude.hashWithSalt` originAccessControlConfig
      `Prelude.hashWithSalt` id

instance Prelude.NFData OriginAccessControl where
  rnf OriginAccessControl' {..} =
    Prelude.rnf originAccessControlConfig
      `Prelude.seq` Prelude.rnf id
