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
-- Module      : Network.AWS.Kendra.Types.JwtTokenTypeConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.JwtTokenTypeConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.KeyLocation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration information for the JWT token type.
--
-- /See:/ 'newJwtTokenTypeConfiguration' smart constructor.
data JwtTokenTypeConfiguration = JwtTokenTypeConfiguration'
  { -- | The Amazon Resource Name (arn) of the secret.
    secretManagerArn :: Prelude.Maybe Prelude.Text,
    -- | The signing key URL.
    url :: Prelude.Maybe Prelude.Text,
    -- | The group attribute field.
    groupAttributeField :: Prelude.Maybe Prelude.Text,
    -- | The regular expression that identifies the claim.
    claimRegex :: Prelude.Maybe Prelude.Text,
    -- | The user name attribute field.
    userNameAttributeField :: Prelude.Maybe Prelude.Text,
    -- | The issuer of the token.
    issuer :: Prelude.Maybe Prelude.Text,
    -- | The location of the key.
    keyLocation :: KeyLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JwtTokenTypeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretManagerArn', 'jwtTokenTypeConfiguration_secretManagerArn' - The Amazon Resource Name (arn) of the secret.
--
-- 'url', 'jwtTokenTypeConfiguration_url' - The signing key URL.
--
-- 'groupAttributeField', 'jwtTokenTypeConfiguration_groupAttributeField' - The group attribute field.
--
-- 'claimRegex', 'jwtTokenTypeConfiguration_claimRegex' - The regular expression that identifies the claim.
--
-- 'userNameAttributeField', 'jwtTokenTypeConfiguration_userNameAttributeField' - The user name attribute field.
--
-- 'issuer', 'jwtTokenTypeConfiguration_issuer' - The issuer of the token.
--
-- 'keyLocation', 'jwtTokenTypeConfiguration_keyLocation' - The location of the key.
newJwtTokenTypeConfiguration ::
  -- | 'keyLocation'
  KeyLocation ->
  JwtTokenTypeConfiguration
newJwtTokenTypeConfiguration pKeyLocation_ =
  JwtTokenTypeConfiguration'
    { secretManagerArn =
        Prelude.Nothing,
      url = Prelude.Nothing,
      groupAttributeField = Prelude.Nothing,
      claimRegex = Prelude.Nothing,
      userNameAttributeField = Prelude.Nothing,
      issuer = Prelude.Nothing,
      keyLocation = pKeyLocation_
    }

-- | The Amazon Resource Name (arn) of the secret.
jwtTokenTypeConfiguration_secretManagerArn :: Lens.Lens' JwtTokenTypeConfiguration (Prelude.Maybe Prelude.Text)
jwtTokenTypeConfiguration_secretManagerArn = Lens.lens (\JwtTokenTypeConfiguration' {secretManagerArn} -> secretManagerArn) (\s@JwtTokenTypeConfiguration' {} a -> s {secretManagerArn = a} :: JwtTokenTypeConfiguration)

-- | The signing key URL.
jwtTokenTypeConfiguration_url :: Lens.Lens' JwtTokenTypeConfiguration (Prelude.Maybe Prelude.Text)
jwtTokenTypeConfiguration_url = Lens.lens (\JwtTokenTypeConfiguration' {url} -> url) (\s@JwtTokenTypeConfiguration' {} a -> s {url = a} :: JwtTokenTypeConfiguration)

-- | The group attribute field.
jwtTokenTypeConfiguration_groupAttributeField :: Lens.Lens' JwtTokenTypeConfiguration (Prelude.Maybe Prelude.Text)
jwtTokenTypeConfiguration_groupAttributeField = Lens.lens (\JwtTokenTypeConfiguration' {groupAttributeField} -> groupAttributeField) (\s@JwtTokenTypeConfiguration' {} a -> s {groupAttributeField = a} :: JwtTokenTypeConfiguration)

-- | The regular expression that identifies the claim.
jwtTokenTypeConfiguration_claimRegex :: Lens.Lens' JwtTokenTypeConfiguration (Prelude.Maybe Prelude.Text)
jwtTokenTypeConfiguration_claimRegex = Lens.lens (\JwtTokenTypeConfiguration' {claimRegex} -> claimRegex) (\s@JwtTokenTypeConfiguration' {} a -> s {claimRegex = a} :: JwtTokenTypeConfiguration)

-- | The user name attribute field.
jwtTokenTypeConfiguration_userNameAttributeField :: Lens.Lens' JwtTokenTypeConfiguration (Prelude.Maybe Prelude.Text)
jwtTokenTypeConfiguration_userNameAttributeField = Lens.lens (\JwtTokenTypeConfiguration' {userNameAttributeField} -> userNameAttributeField) (\s@JwtTokenTypeConfiguration' {} a -> s {userNameAttributeField = a} :: JwtTokenTypeConfiguration)

-- | The issuer of the token.
jwtTokenTypeConfiguration_issuer :: Lens.Lens' JwtTokenTypeConfiguration (Prelude.Maybe Prelude.Text)
jwtTokenTypeConfiguration_issuer = Lens.lens (\JwtTokenTypeConfiguration' {issuer} -> issuer) (\s@JwtTokenTypeConfiguration' {} a -> s {issuer = a} :: JwtTokenTypeConfiguration)

-- | The location of the key.
jwtTokenTypeConfiguration_keyLocation :: Lens.Lens' JwtTokenTypeConfiguration KeyLocation
jwtTokenTypeConfiguration_keyLocation = Lens.lens (\JwtTokenTypeConfiguration' {keyLocation} -> keyLocation) (\s@JwtTokenTypeConfiguration' {} a -> s {keyLocation = a} :: JwtTokenTypeConfiguration)

instance Core.FromJSON JwtTokenTypeConfiguration where
  parseJSON =
    Core.withObject
      "JwtTokenTypeConfiguration"
      ( \x ->
          JwtTokenTypeConfiguration'
            Prelude.<$> (x Core..:? "SecretManagerArn")
            Prelude.<*> (x Core..:? "URL")
            Prelude.<*> (x Core..:? "GroupAttributeField")
            Prelude.<*> (x Core..:? "ClaimRegex")
            Prelude.<*> (x Core..:? "UserNameAttributeField")
            Prelude.<*> (x Core..:? "Issuer")
            Prelude.<*> (x Core..: "KeyLocation")
      )

instance Prelude.Hashable JwtTokenTypeConfiguration

instance Prelude.NFData JwtTokenTypeConfiguration

instance Core.ToJSON JwtTokenTypeConfiguration where
  toJSON JwtTokenTypeConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecretManagerArn" Core..=)
              Prelude.<$> secretManagerArn,
            ("URL" Core..=) Prelude.<$> url,
            ("GroupAttributeField" Core..=)
              Prelude.<$> groupAttributeField,
            ("ClaimRegex" Core..=) Prelude.<$> claimRegex,
            ("UserNameAttributeField" Core..=)
              Prelude.<$> userNameAttributeField,
            ("Issuer" Core..=) Prelude.<$> issuer,
            Prelude.Just ("KeyLocation" Core..= keyLocation)
          ]
      )
