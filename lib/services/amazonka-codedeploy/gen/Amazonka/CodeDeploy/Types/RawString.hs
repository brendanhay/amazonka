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
-- Module      : Amazonka.CodeDeploy.Types.RawString
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.RawString where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A revision for an Lambda deployment that is a YAML-formatted or
-- JSON-formatted string. For Lambda deployments, the revision is the same
-- as the AppSpec file.
--
-- /See:/ 'newRawString' smart constructor.
data RawString = RawString'
  { -- | The YAML-formatted or JSON-formatted revision string. It includes
    -- information about which Lambda function to update and optional Lambda
    -- functions that validate deployment lifecycle events.
    content :: Prelude.Maybe Prelude.Text,
    -- | The SHA256 hash value of the revision content.
    sha256 :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RawString' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'rawString_content' - The YAML-formatted or JSON-formatted revision string. It includes
-- information about which Lambda function to update and optional Lambda
-- functions that validate deployment lifecycle events.
--
-- 'sha256', 'rawString_sha256' - The SHA256 hash value of the revision content.
newRawString ::
  RawString
newRawString =
  RawString'
    { content = Prelude.Nothing,
      sha256 = Prelude.Nothing
    }

-- | The YAML-formatted or JSON-formatted revision string. It includes
-- information about which Lambda function to update and optional Lambda
-- functions that validate deployment lifecycle events.
rawString_content :: Lens.Lens' RawString (Prelude.Maybe Prelude.Text)
rawString_content = Lens.lens (\RawString' {content} -> content) (\s@RawString' {} a -> s {content = a} :: RawString)

-- | The SHA256 hash value of the revision content.
rawString_sha256 :: Lens.Lens' RawString (Prelude.Maybe Prelude.Text)
rawString_sha256 = Lens.lens (\RawString' {sha256} -> sha256) (\s@RawString' {} a -> s {sha256 = a} :: RawString)

instance Data.FromJSON RawString where
  parseJSON =
    Data.withObject
      "RawString"
      ( \x ->
          RawString'
            Prelude.<$> (x Data..:? "content")
            Prelude.<*> (x Data..:? "sha256")
      )

instance Prelude.Hashable RawString where
  hashWithSalt _salt RawString' {..} =
    _salt
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` sha256

instance Prelude.NFData RawString where
  rnf RawString' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf sha256

instance Data.ToJSON RawString where
  toJSON RawString' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("content" Data..=) Prelude.<$> content,
            ("sha256" Data..=) Prelude.<$> sha256
          ]
      )
