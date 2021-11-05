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
-- Module      : Network.AWS.KafkaConnect.Types.CustomPluginRevisionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KafkaConnect.Types.CustomPluginRevisionSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.KafkaConnect.Types.CustomPluginContentType
import Network.AWS.KafkaConnect.Types.CustomPluginFileDescription
import Network.AWS.KafkaConnect.Types.CustomPluginLocationDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about the revision of a custom plugin.
--
-- /See:/ 'newCustomPluginRevisionSummary' smart constructor.
data CustomPluginRevisionSummary = CustomPluginRevisionSummary'
  { -- | The time that the custom plugin was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Information about the location of the custom plugin.
    location :: Prelude.Maybe CustomPluginLocationDescription,
    -- | Details about the custom plugin file.
    fileDescription :: Prelude.Maybe CustomPluginFileDescription,
    -- | The revision of the custom plugin.
    revision :: Prelude.Maybe Prelude.Integer,
    -- | The description of the custom plugin.
    description :: Prelude.Maybe Prelude.Text,
    -- | The format of the plugin file.
    contentType :: Prelude.Maybe CustomPluginContentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomPluginRevisionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'customPluginRevisionSummary_creationTime' - The time that the custom plugin was created.
--
-- 'location', 'customPluginRevisionSummary_location' - Information about the location of the custom plugin.
--
-- 'fileDescription', 'customPluginRevisionSummary_fileDescription' - Details about the custom plugin file.
--
-- 'revision', 'customPluginRevisionSummary_revision' - The revision of the custom plugin.
--
-- 'description', 'customPluginRevisionSummary_description' - The description of the custom plugin.
--
-- 'contentType', 'customPluginRevisionSummary_contentType' - The format of the plugin file.
newCustomPluginRevisionSummary ::
  CustomPluginRevisionSummary
newCustomPluginRevisionSummary =
  CustomPluginRevisionSummary'
    { creationTime =
        Prelude.Nothing,
      location = Prelude.Nothing,
      fileDescription = Prelude.Nothing,
      revision = Prelude.Nothing,
      description = Prelude.Nothing,
      contentType = Prelude.Nothing
    }

-- | The time that the custom plugin was created.
customPluginRevisionSummary_creationTime :: Lens.Lens' CustomPluginRevisionSummary (Prelude.Maybe Prelude.UTCTime)
customPluginRevisionSummary_creationTime = Lens.lens (\CustomPluginRevisionSummary' {creationTime} -> creationTime) (\s@CustomPluginRevisionSummary' {} a -> s {creationTime = a} :: CustomPluginRevisionSummary) Prelude.. Lens.mapping Core._Time

-- | Information about the location of the custom plugin.
customPluginRevisionSummary_location :: Lens.Lens' CustomPluginRevisionSummary (Prelude.Maybe CustomPluginLocationDescription)
customPluginRevisionSummary_location = Lens.lens (\CustomPluginRevisionSummary' {location} -> location) (\s@CustomPluginRevisionSummary' {} a -> s {location = a} :: CustomPluginRevisionSummary)

-- | Details about the custom plugin file.
customPluginRevisionSummary_fileDescription :: Lens.Lens' CustomPluginRevisionSummary (Prelude.Maybe CustomPluginFileDescription)
customPluginRevisionSummary_fileDescription = Lens.lens (\CustomPluginRevisionSummary' {fileDescription} -> fileDescription) (\s@CustomPluginRevisionSummary' {} a -> s {fileDescription = a} :: CustomPluginRevisionSummary)

-- | The revision of the custom plugin.
customPluginRevisionSummary_revision :: Lens.Lens' CustomPluginRevisionSummary (Prelude.Maybe Prelude.Integer)
customPluginRevisionSummary_revision = Lens.lens (\CustomPluginRevisionSummary' {revision} -> revision) (\s@CustomPluginRevisionSummary' {} a -> s {revision = a} :: CustomPluginRevisionSummary)

-- | The description of the custom plugin.
customPluginRevisionSummary_description :: Lens.Lens' CustomPluginRevisionSummary (Prelude.Maybe Prelude.Text)
customPluginRevisionSummary_description = Lens.lens (\CustomPluginRevisionSummary' {description} -> description) (\s@CustomPluginRevisionSummary' {} a -> s {description = a} :: CustomPluginRevisionSummary)

-- | The format of the plugin file.
customPluginRevisionSummary_contentType :: Lens.Lens' CustomPluginRevisionSummary (Prelude.Maybe CustomPluginContentType)
customPluginRevisionSummary_contentType = Lens.lens (\CustomPluginRevisionSummary' {contentType} -> contentType) (\s@CustomPluginRevisionSummary' {} a -> s {contentType = a} :: CustomPluginRevisionSummary)

instance Core.FromJSON CustomPluginRevisionSummary where
  parseJSON =
    Core.withObject
      "CustomPluginRevisionSummary"
      ( \x ->
          CustomPluginRevisionSummary'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "fileDescription")
            Prelude.<*> (x Core..:? "revision")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "contentType")
      )

instance Prelude.Hashable CustomPluginRevisionSummary

instance Prelude.NFData CustomPluginRevisionSummary
