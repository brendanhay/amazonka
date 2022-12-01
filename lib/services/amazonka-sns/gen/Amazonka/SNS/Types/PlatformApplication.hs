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
-- Module      : Amazonka.SNS.Types.PlatformApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SNS.Types.PlatformApplication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Platform application object.
--
-- /See:/ 'newPlatformApplication' smart constructor.
data PlatformApplication = PlatformApplication'
  { -- | PlatformApplicationArn for platform application object.
    platformApplicationArn :: Prelude.Maybe Prelude.Text,
    -- | Attributes for platform application object.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlatformApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformApplicationArn', 'platformApplication_platformApplicationArn' - PlatformApplicationArn for platform application object.
--
-- 'attributes', 'platformApplication_attributes' - Attributes for platform application object.
newPlatformApplication ::
  PlatformApplication
newPlatformApplication =
  PlatformApplication'
    { platformApplicationArn =
        Prelude.Nothing,
      attributes = Prelude.Nothing
    }

-- | PlatformApplicationArn for platform application object.
platformApplication_platformApplicationArn :: Lens.Lens' PlatformApplication (Prelude.Maybe Prelude.Text)
platformApplication_platformApplicationArn = Lens.lens (\PlatformApplication' {platformApplicationArn} -> platformApplicationArn) (\s@PlatformApplication' {} a -> s {platformApplicationArn = a} :: PlatformApplication)

-- | Attributes for platform application object.
platformApplication_attributes :: Lens.Lens' PlatformApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
platformApplication_attributes = Lens.lens (\PlatformApplication' {attributes} -> attributes) (\s@PlatformApplication' {} a -> s {attributes = a} :: PlatformApplication) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML PlatformApplication where
  parseXML x =
    PlatformApplication'
      Prelude.<$> (x Core..@? "PlatformApplicationArn")
      Prelude.<*> ( x Core..@? "Attributes" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLMap "entry" "key" "value")
                  )

instance Prelude.Hashable PlatformApplication where
  hashWithSalt _salt PlatformApplication' {..} =
    _salt `Prelude.hashWithSalt` platformApplicationArn
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData PlatformApplication where
  rnf PlatformApplication' {..} =
    Prelude.rnf platformApplicationArn
      `Prelude.seq` Prelude.rnf attributes
