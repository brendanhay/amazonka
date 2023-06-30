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
-- Module      : Amazonka.IoT.Types.DocumentParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.DocumentParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A map of key-value pairs containing the patterns that need to be
-- replaced in a managed template job document schema. You can use the
-- description of each key as a guidance to specify the inputs during
-- runtime when creating a job.
--
-- @documentParameters@ can only be used when creating jobs from Amazon Web
-- Services managed templates. This parameter can\'t be used with custom
-- job templates or to create jobs from them.
--
-- /See:/ 'newDocumentParameter' smart constructor.
data DocumentParameter = DocumentParameter'
  { -- | Description of the map field containing the patterns that need to be
    -- replaced in a managed template job document schema.
    description :: Prelude.Maybe Prelude.Text,
    -- | An example illustrating a pattern that need to be replaced in a managed
    -- template job document schema.
    example :: Prelude.Maybe Prelude.Text,
    -- | Key of the map field containing the patterns that need to be replaced in
    -- a managed template job document schema.
    key :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether a pattern that needs to be replaced in a managed
    -- template job document schema is optional or required.
    optional :: Prelude.Maybe Prelude.Bool,
    -- | A regular expression of the patterns that need to be replaced in a
    -- managed template job document schema.
    regex :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'documentParameter_description' - Description of the map field containing the patterns that need to be
-- replaced in a managed template job document schema.
--
-- 'example', 'documentParameter_example' - An example illustrating a pattern that need to be replaced in a managed
-- template job document schema.
--
-- 'key', 'documentParameter_key' - Key of the map field containing the patterns that need to be replaced in
-- a managed template job document schema.
--
-- 'optional', 'documentParameter_optional' - Specifies whether a pattern that needs to be replaced in a managed
-- template job document schema is optional or required.
--
-- 'regex', 'documentParameter_regex' - A regular expression of the patterns that need to be replaced in a
-- managed template job document schema.
newDocumentParameter ::
  DocumentParameter
newDocumentParameter =
  DocumentParameter'
    { description = Prelude.Nothing,
      example = Prelude.Nothing,
      key = Prelude.Nothing,
      optional = Prelude.Nothing,
      regex = Prelude.Nothing
    }

-- | Description of the map field containing the patterns that need to be
-- replaced in a managed template job document schema.
documentParameter_description :: Lens.Lens' DocumentParameter (Prelude.Maybe Prelude.Text)
documentParameter_description = Lens.lens (\DocumentParameter' {description} -> description) (\s@DocumentParameter' {} a -> s {description = a} :: DocumentParameter)

-- | An example illustrating a pattern that need to be replaced in a managed
-- template job document schema.
documentParameter_example :: Lens.Lens' DocumentParameter (Prelude.Maybe Prelude.Text)
documentParameter_example = Lens.lens (\DocumentParameter' {example} -> example) (\s@DocumentParameter' {} a -> s {example = a} :: DocumentParameter)

-- | Key of the map field containing the patterns that need to be replaced in
-- a managed template job document schema.
documentParameter_key :: Lens.Lens' DocumentParameter (Prelude.Maybe Prelude.Text)
documentParameter_key = Lens.lens (\DocumentParameter' {key} -> key) (\s@DocumentParameter' {} a -> s {key = a} :: DocumentParameter)

-- | Specifies whether a pattern that needs to be replaced in a managed
-- template job document schema is optional or required.
documentParameter_optional :: Lens.Lens' DocumentParameter (Prelude.Maybe Prelude.Bool)
documentParameter_optional = Lens.lens (\DocumentParameter' {optional} -> optional) (\s@DocumentParameter' {} a -> s {optional = a} :: DocumentParameter)

-- | A regular expression of the patterns that need to be replaced in a
-- managed template job document schema.
documentParameter_regex :: Lens.Lens' DocumentParameter (Prelude.Maybe Prelude.Text)
documentParameter_regex = Lens.lens (\DocumentParameter' {regex} -> regex) (\s@DocumentParameter' {} a -> s {regex = a} :: DocumentParameter)

instance Data.FromJSON DocumentParameter where
  parseJSON =
    Data.withObject
      "DocumentParameter"
      ( \x ->
          DocumentParameter'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "example")
            Prelude.<*> (x Data..:? "key")
            Prelude.<*> (x Data..:? "optional")
            Prelude.<*> (x Data..:? "regex")
      )

instance Prelude.Hashable DocumentParameter where
  hashWithSalt _salt DocumentParameter' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` example
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` optional
      `Prelude.hashWithSalt` regex

instance Prelude.NFData DocumentParameter where
  rnf DocumentParameter' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf example
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf optional
      `Prelude.seq` Prelude.rnf regex
