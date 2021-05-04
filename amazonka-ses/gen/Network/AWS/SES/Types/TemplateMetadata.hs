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
-- Module      : Network.AWS.SES.Types.TemplateMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.TemplateMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about an email template.
--
-- /See:/ 'newTemplateMetadata' smart constructor.
data TemplateMetadata = TemplateMetadata'
  { -- | The time and date the template was created.
    createdTimestamp :: Prelude.Maybe Prelude.ISO8601,
    -- | The name of the template.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { createdTimestamp =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The time and date the template was created.
templateMetadata_createdTimestamp :: Lens.Lens' TemplateMetadata (Prelude.Maybe Prelude.UTCTime)
templateMetadata_createdTimestamp = Lens.lens (\TemplateMetadata' {createdTimestamp} -> createdTimestamp) (\s@TemplateMetadata' {} a -> s {createdTimestamp = a} :: TemplateMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The name of the template.
templateMetadata_name :: Lens.Lens' TemplateMetadata (Prelude.Maybe Prelude.Text)
templateMetadata_name = Lens.lens (\TemplateMetadata' {name} -> name) (\s@TemplateMetadata' {} a -> s {name = a} :: TemplateMetadata)

instance Prelude.FromXML TemplateMetadata where
  parseXML x =
    TemplateMetadata'
      Prelude.<$> (x Prelude..@? "CreatedTimestamp")
      Prelude.<*> (x Prelude..@? "Name")

instance Prelude.Hashable TemplateMetadata

instance Prelude.NFData TemplateMetadata
