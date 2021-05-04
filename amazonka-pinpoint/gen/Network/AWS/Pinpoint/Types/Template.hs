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
-- Module      : Network.AWS.Pinpoint.Types.Template
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Template where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the name and version of the message template to use for the
-- message.
--
-- /See:/ 'newTemplate' smart constructor.
data Template = Template'
  { -- | The unique identifier for the version of the message template to use for
    -- the message. If specified, this value must match the identifier for an
    -- existing template version. To retrieve a list of versions and version
    -- identifiers for a template, use the Template Versions resource.
    --
    -- If you don\'t specify a value for this property, Amazon Pinpoint uses
    -- the /active version/ of the template. The /active version/ is typically
    -- the version of a template that\'s been most recently reviewed and
    -- approved for use, depending on your workflow. It isn\'t necessarily the
    -- latest version of a template.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the message template to use for the message. If specified,
    -- this value must match the name of an existing message template.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Template' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'template_version' - The unique identifier for the version of the message template to use for
-- the message. If specified, this value must match the identifier for an
-- existing template version. To retrieve a list of versions and version
-- identifiers for a template, use the Template Versions resource.
--
-- If you don\'t specify a value for this property, Amazon Pinpoint uses
-- the /active version/ of the template. The /active version/ is typically
-- the version of a template that\'s been most recently reviewed and
-- approved for use, depending on your workflow. It isn\'t necessarily the
-- latest version of a template.
--
-- 'name', 'template_name' - The name of the message template to use for the message. If specified,
-- this value must match the name of an existing message template.
newTemplate ::
  Template
newTemplate =
  Template'
    { version = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The unique identifier for the version of the message template to use for
-- the message. If specified, this value must match the identifier for an
-- existing template version. To retrieve a list of versions and version
-- identifiers for a template, use the Template Versions resource.
--
-- If you don\'t specify a value for this property, Amazon Pinpoint uses
-- the /active version/ of the template. The /active version/ is typically
-- the version of a template that\'s been most recently reviewed and
-- approved for use, depending on your workflow. It isn\'t necessarily the
-- latest version of a template.
template_version :: Lens.Lens' Template (Prelude.Maybe Prelude.Text)
template_version = Lens.lens (\Template' {version} -> version) (\s@Template' {} a -> s {version = a} :: Template)

-- | The name of the message template to use for the message. If specified,
-- this value must match the name of an existing message template.
template_name :: Lens.Lens' Template (Prelude.Maybe Prelude.Text)
template_name = Lens.lens (\Template' {name} -> name) (\s@Template' {} a -> s {name = a} :: Template)

instance Prelude.FromJSON Template where
  parseJSON =
    Prelude.withObject
      "Template"
      ( \x ->
          Template'
            Prelude.<$> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable Template

instance Prelude.NFData Template

instance Prelude.ToJSON Template where
  toJSON Template' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Version" Prelude..=) Prelude.<$> version,
            ("Name" Prelude..=) Prelude.<$> name
          ]
      )
