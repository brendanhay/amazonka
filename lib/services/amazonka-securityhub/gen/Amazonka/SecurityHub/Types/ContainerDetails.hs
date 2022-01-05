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
-- Module      : Amazonka.SecurityHub.Types.ContainerDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ContainerDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Container details related to a finding.
--
-- /See:/ 'newContainerDetails' smart constructor.
data ContainerDetails = ContainerDetails'
  { -- | The name of the container related to a finding.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the image related to a finding.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The name of the image related to a finding.
    imageName :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the container started.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    launchedAt :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'containerDetails_name' - The name of the container related to a finding.
--
-- 'imageId', 'containerDetails_imageId' - The identifier of the image related to a finding.
--
-- 'imageName', 'containerDetails_imageName' - The name of the image related to a finding.
--
-- 'launchedAt', 'containerDetails_launchedAt' - Indicates when the container started.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
newContainerDetails ::
  ContainerDetails
newContainerDetails =
  ContainerDetails'
    { name = Prelude.Nothing,
      imageId = Prelude.Nothing,
      imageName = Prelude.Nothing,
      launchedAt = Prelude.Nothing
    }

-- | The name of the container related to a finding.
containerDetails_name :: Lens.Lens' ContainerDetails (Prelude.Maybe Prelude.Text)
containerDetails_name = Lens.lens (\ContainerDetails' {name} -> name) (\s@ContainerDetails' {} a -> s {name = a} :: ContainerDetails)

-- | The identifier of the image related to a finding.
containerDetails_imageId :: Lens.Lens' ContainerDetails (Prelude.Maybe Prelude.Text)
containerDetails_imageId = Lens.lens (\ContainerDetails' {imageId} -> imageId) (\s@ContainerDetails' {} a -> s {imageId = a} :: ContainerDetails)

-- | The name of the image related to a finding.
containerDetails_imageName :: Lens.Lens' ContainerDetails (Prelude.Maybe Prelude.Text)
containerDetails_imageName = Lens.lens (\ContainerDetails' {imageName} -> imageName) (\s@ContainerDetails' {} a -> s {imageName = a} :: ContainerDetails)

-- | Indicates when the container started.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
containerDetails_launchedAt :: Lens.Lens' ContainerDetails (Prelude.Maybe Prelude.Text)
containerDetails_launchedAt = Lens.lens (\ContainerDetails' {launchedAt} -> launchedAt) (\s@ContainerDetails' {} a -> s {launchedAt = a} :: ContainerDetails)

instance Core.FromJSON ContainerDetails where
  parseJSON =
    Core.withObject
      "ContainerDetails"
      ( \x ->
          ContainerDetails'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ImageId")
            Prelude.<*> (x Core..:? "ImageName")
            Prelude.<*> (x Core..:? "LaunchedAt")
      )

instance Prelude.Hashable ContainerDetails where
  hashWithSalt _salt ContainerDetails' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` imageName
      `Prelude.hashWithSalt` launchedAt

instance Prelude.NFData ContainerDetails where
  rnf ContainerDetails' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf imageName
      `Prelude.seq` Prelude.rnf launchedAt

instance Core.ToJSON ContainerDetails where
  toJSON ContainerDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("ImageId" Core..=) Prelude.<$> imageId,
            ("ImageName" Core..=) Prelude.<$> imageName,
            ("LaunchedAt" Core..=) Prelude.<$> launchedAt
          ]
      )
