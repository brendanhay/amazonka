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
-- Module      : Network.AWS.SES.Types.TemplateMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.TemplateMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about an email template.
--
-- /See:/ 'newTemplateMetadata' smart constructor.
data TemplateMetadata = TemplateMetadata'
  { -- | The time and date the template was created.
    createdTimestamp :: Core.Maybe Core.ISO8601,
    -- | The name of the template.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TemplateMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'templateMetadata_createdTimestamp' - The time and date the template was created.
--
-- 'name', 'templateMetadata_name' - The name of the template.
newTemplateMetadata ::
  TemplateMetadata
newTemplateMetadata =
  TemplateMetadata'
    { createdTimestamp = Core.Nothing,
      name = Core.Nothing
    }

-- | The time and date the template was created.
templateMetadata_createdTimestamp :: Lens.Lens' TemplateMetadata (Core.Maybe Core.UTCTime)
templateMetadata_createdTimestamp = Lens.lens (\TemplateMetadata' {createdTimestamp} -> createdTimestamp) (\s@TemplateMetadata' {} a -> s {createdTimestamp = a} :: TemplateMetadata) Core.. Lens.mapping Core._Time

-- | The name of the template.
templateMetadata_name :: Lens.Lens' TemplateMetadata (Core.Maybe Core.Text)
templateMetadata_name = Lens.lens (\TemplateMetadata' {name} -> name) (\s@TemplateMetadata' {} a -> s {name = a} :: TemplateMetadata)

instance Core.FromXML TemplateMetadata where
  parseXML x =
    TemplateMetadata'
      Core.<$> (x Core..@? "CreatedTimestamp")
      Core.<*> (x Core..@? "Name")

instance Core.Hashable TemplateMetadata

instance Core.NFData TemplateMetadata
