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
-- Module      : Amazonka.AlexaBusiness.Types.DeveloperInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.DeveloperInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details about the developer that published the skill.
--
-- /See:/ 'newDeveloperInfo' smart constructor.
data DeveloperInfo = DeveloperInfo'
  { -- | The email of the developer.
    email :: Prelude.Maybe Prelude.Text,
    -- | The name of the developer.
    developerName :: Prelude.Maybe Prelude.Text,
    -- | The website of the developer.
    url :: Prelude.Maybe Prelude.Text,
    -- | The URL of the privacy policy.
    privacyPolicy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeveloperInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'email', 'developerInfo_email' - The email of the developer.
--
-- 'developerName', 'developerInfo_developerName' - The name of the developer.
--
-- 'url', 'developerInfo_url' - The website of the developer.
--
-- 'privacyPolicy', 'developerInfo_privacyPolicy' - The URL of the privacy policy.
newDeveloperInfo ::
  DeveloperInfo
newDeveloperInfo =
  DeveloperInfo'
    { email = Prelude.Nothing,
      developerName = Prelude.Nothing,
      url = Prelude.Nothing,
      privacyPolicy = Prelude.Nothing
    }

-- | The email of the developer.
developerInfo_email :: Lens.Lens' DeveloperInfo (Prelude.Maybe Prelude.Text)
developerInfo_email = Lens.lens (\DeveloperInfo' {email} -> email) (\s@DeveloperInfo' {} a -> s {email = a} :: DeveloperInfo)

-- | The name of the developer.
developerInfo_developerName :: Lens.Lens' DeveloperInfo (Prelude.Maybe Prelude.Text)
developerInfo_developerName = Lens.lens (\DeveloperInfo' {developerName} -> developerName) (\s@DeveloperInfo' {} a -> s {developerName = a} :: DeveloperInfo)

-- | The website of the developer.
developerInfo_url :: Lens.Lens' DeveloperInfo (Prelude.Maybe Prelude.Text)
developerInfo_url = Lens.lens (\DeveloperInfo' {url} -> url) (\s@DeveloperInfo' {} a -> s {url = a} :: DeveloperInfo)

-- | The URL of the privacy policy.
developerInfo_privacyPolicy :: Lens.Lens' DeveloperInfo (Prelude.Maybe Prelude.Text)
developerInfo_privacyPolicy = Lens.lens (\DeveloperInfo' {privacyPolicy} -> privacyPolicy) (\s@DeveloperInfo' {} a -> s {privacyPolicy = a} :: DeveloperInfo)

instance Data.FromJSON DeveloperInfo where
  parseJSON =
    Data.withObject
      "DeveloperInfo"
      ( \x ->
          DeveloperInfo'
            Prelude.<$> (x Data..:? "Email")
            Prelude.<*> (x Data..:? "DeveloperName")
            Prelude.<*> (x Data..:? "Url")
            Prelude.<*> (x Data..:? "PrivacyPolicy")
      )

instance Prelude.Hashable DeveloperInfo where
  hashWithSalt _salt DeveloperInfo' {..} =
    _salt `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` developerName
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` privacyPolicy

instance Prelude.NFData DeveloperInfo where
  rnf DeveloperInfo' {..} =
    Prelude.rnf email
      `Prelude.seq` Prelude.rnf developerName
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf privacyPolicy
