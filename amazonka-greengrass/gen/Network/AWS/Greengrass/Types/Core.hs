{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Greengrass.Types.Core
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Core where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a core.
--
-- /See:/ 'newCore' smart constructor.
data Core = Core'
  { -- | If true, the core\'s local shadow is automatically synced with the
    -- cloud.
    syncShadow :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the thing which is the core.
    thingArn :: Prelude.Text,
    -- | A descriptive or arbitrary ID for the core. This value must be unique
    -- within the core definition version. Max length is 128 characters with
    -- pattern \'\'[a-zA-Z0-9:_-]+\'\'.
    id :: Prelude.Text,
    -- | The ARN of the certificate associated with the core.
    certificateArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Core' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'syncShadow', 'core_syncShadow' - If true, the core\'s local shadow is automatically synced with the
-- cloud.
--
-- 'thingArn', 'core_thingArn' - The ARN of the thing which is the core.
--
-- 'id', 'core_id' - A descriptive or arbitrary ID for the core. This value must be unique
-- within the core definition version. Max length is 128 characters with
-- pattern \'\'[a-zA-Z0-9:_-]+\'\'.
--
-- 'certificateArn', 'core_certificateArn' - The ARN of the certificate associated with the core.
newCore ::
  -- | 'thingArn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'certificateArn'
  Prelude.Text ->
  Core
newCore pThingArn_ pId_ pCertificateArn_ =
  Core'
    { syncShadow = Prelude.Nothing,
      thingArn = pThingArn_,
      id = pId_,
      certificateArn = pCertificateArn_
    }

-- | If true, the core\'s local shadow is automatically synced with the
-- cloud.
core_syncShadow :: Lens.Lens' Core (Prelude.Maybe Prelude.Bool)
core_syncShadow = Lens.lens (\Core' {syncShadow} -> syncShadow) (\s@Core' {} a -> s {syncShadow = a} :: Core)

-- | The ARN of the thing which is the core.
core_thingArn :: Lens.Lens' Core Prelude.Text
core_thingArn = Lens.lens (\Core' {thingArn} -> thingArn) (\s@Core' {} a -> s {thingArn = a} :: Core)

-- | A descriptive or arbitrary ID for the core. This value must be unique
-- within the core definition version. Max length is 128 characters with
-- pattern \'\'[a-zA-Z0-9:_-]+\'\'.
core_id :: Lens.Lens' Core Prelude.Text
core_id = Lens.lens (\Core' {id} -> id) (\s@Core' {} a -> s {id = a} :: Core)

-- | The ARN of the certificate associated with the core.
core_certificateArn :: Lens.Lens' Core Prelude.Text
core_certificateArn = Lens.lens (\Core' {certificateArn} -> certificateArn) (\s@Core' {} a -> s {certificateArn = a} :: Core)

instance Prelude.FromJSON Core where
  parseJSON =
    Prelude.withObject
      "Core"
      ( \x ->
          Core'
            Prelude.<$> (x Prelude..:? "SyncShadow")
            Prelude.<*> (x Prelude..: "ThingArn")
            Prelude.<*> (x Prelude..: "Id")
            Prelude.<*> (x Prelude..: "CertificateArn")
      )

instance Prelude.Hashable Core

instance Prelude.NFData Core

instance Prelude.ToJSON Core where
  toJSON Core' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SyncShadow" Prelude..=) Prelude.<$> syncShadow,
            Prelude.Just ("ThingArn" Prelude..= thingArn),
            Prelude.Just ("Id" Prelude..= id),
            Prelude.Just
              ("CertificateArn" Prelude..= certificateArn)
          ]
      )
