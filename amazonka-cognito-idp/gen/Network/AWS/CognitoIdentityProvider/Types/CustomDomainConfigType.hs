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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The configuration for a custom domain that hosts the sign-up and sign-in
-- webpages for your application.
--
-- /See:/ 'newCustomDomainConfigType' smart constructor.
data CustomDomainConfigType = CustomDomainConfigType'
  { -- | The Amazon Resource Name (ARN) of an AWS Certificate Manager SSL
    -- certificate. You use this certificate for the subdomain of your custom
    -- domain.
    certificateArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CustomDomainConfigType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'customDomainConfigType_certificateArn' - The Amazon Resource Name (ARN) of an AWS Certificate Manager SSL
-- certificate. You use this certificate for the subdomain of your custom
-- domain.
newCustomDomainConfigType ::
  -- | 'certificateArn'
  Core.Text ->
  CustomDomainConfigType
newCustomDomainConfigType pCertificateArn_ =
  CustomDomainConfigType'
    { certificateArn =
        pCertificateArn_
    }

-- | The Amazon Resource Name (ARN) of an AWS Certificate Manager SSL
-- certificate. You use this certificate for the subdomain of your custom
-- domain.
customDomainConfigType_certificateArn :: Lens.Lens' CustomDomainConfigType Core.Text
customDomainConfigType_certificateArn = Lens.lens (\CustomDomainConfigType' {certificateArn} -> certificateArn) (\s@CustomDomainConfigType' {} a -> s {certificateArn = a} :: CustomDomainConfigType)

instance Core.FromJSON CustomDomainConfigType where
  parseJSON =
    Core.withObject
      "CustomDomainConfigType"
      ( \x ->
          CustomDomainConfigType'
            Core.<$> (x Core..: "CertificateArn")
      )

instance Core.Hashable CustomDomainConfigType

instance Core.NFData CustomDomainConfigType

instance Core.ToJSON CustomDomainConfigType where
  toJSON CustomDomainConfigType' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CertificateArn" Core..= certificateArn)
          ]
      )
