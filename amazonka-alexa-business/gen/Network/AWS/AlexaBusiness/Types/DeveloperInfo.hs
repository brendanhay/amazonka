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
-- Module      : Network.AWS.AlexaBusiness.Types.DeveloperInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeveloperInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details about the developer that published the skill.
--
-- /See:/ 'newDeveloperInfo' smart constructor.
data DeveloperInfo = DeveloperInfo'
  { -- | The name of the developer.
    developerName :: Prelude.Maybe Prelude.Text,
    -- | The email of the developer.
    email :: Prelude.Maybe Prelude.Text,
    -- | The URL of the privacy policy.
    privacyPolicy :: Prelude.Maybe Prelude.Text,
    -- | The website of the developer.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeveloperInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'developerName', 'developerInfo_developerName' - The name of the developer.
--
-- 'email', 'developerInfo_email' - The email of the developer.
--
-- 'privacyPolicy', 'developerInfo_privacyPolicy' - The URL of the privacy policy.
--
-- 'url', 'developerInfo_url' - The website of the developer.
newDeveloperInfo ::
  DeveloperInfo
newDeveloperInfo =
  DeveloperInfo'
    { developerName = Prelude.Nothing,
      email = Prelude.Nothing,
      privacyPolicy = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The name of the developer.
developerInfo_developerName :: Lens.Lens' DeveloperInfo (Prelude.Maybe Prelude.Text)
developerInfo_developerName = Lens.lens (\DeveloperInfo' {developerName} -> developerName) (\s@DeveloperInfo' {} a -> s {developerName = a} :: DeveloperInfo)

-- | The email of the developer.
developerInfo_email :: Lens.Lens' DeveloperInfo (Prelude.Maybe Prelude.Text)
developerInfo_email = Lens.lens (\DeveloperInfo' {email} -> email) (\s@DeveloperInfo' {} a -> s {email = a} :: DeveloperInfo)

-- | The URL of the privacy policy.
developerInfo_privacyPolicy :: Lens.Lens' DeveloperInfo (Prelude.Maybe Prelude.Text)
developerInfo_privacyPolicy = Lens.lens (\DeveloperInfo' {privacyPolicy} -> privacyPolicy) (\s@DeveloperInfo' {} a -> s {privacyPolicy = a} :: DeveloperInfo)

-- | The website of the developer.
developerInfo_url :: Lens.Lens' DeveloperInfo (Prelude.Maybe Prelude.Text)
developerInfo_url = Lens.lens (\DeveloperInfo' {url} -> url) (\s@DeveloperInfo' {} a -> s {url = a} :: DeveloperInfo)

instance Prelude.FromJSON DeveloperInfo where
  parseJSON =
    Prelude.withObject
      "DeveloperInfo"
      ( \x ->
          DeveloperInfo'
            Prelude.<$> (x Prelude..:? "DeveloperName")
            Prelude.<*> (x Prelude..:? "Email")
            Prelude.<*> (x Prelude..:? "PrivacyPolicy")
            Prelude.<*> (x Prelude..:? "Url")
      )

instance Prelude.Hashable DeveloperInfo

instance Prelude.NFData DeveloperInfo
