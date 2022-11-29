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
-- Module      : Amazonka.CloudFront.Types.OriginAccessControlConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginAccessControlConfig where

import Amazonka.CloudFront.Types.OriginAccessControlOriginTypes
import Amazonka.CloudFront.Types.OriginAccessControlSigningBehaviors
import Amazonka.CloudFront.Types.OriginAccessControlSigningProtocols
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A CloudFront origin access control configuration.
--
-- /See:/ 'newOriginAccessControlConfig' smart constructor.
data OriginAccessControlConfig = OriginAccessControlConfig'
  { -- | A description of the origin access control.
    description :: Prelude.Maybe Prelude.Text,
    -- | A name to identify the origin access control.
    name :: Prelude.Text,
    -- | The signing protocol of the origin access control, which determines how
    -- CloudFront signs (authenticates) requests. The only valid value is
    -- @sigv4@.
    signingProtocol :: OriginAccessControlSigningProtocols,
    -- | Specifies which requests CloudFront signs (adds authentication
    -- information to). Specify @always@ for the most common use case. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html#oac-advanced-settings origin access control advanced settings>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- This field can have one of the following values:
    --
    -- -   @always@ – CloudFront signs all origin requests, overwriting the
    --     @Authorization@ header from the viewer request if one exists.
    --
    -- -   @never@ – CloudFront doesn\'t sign any origin requests. This value
    --     turns off origin access control for all origins in all distributions
    --     that use this origin access control.
    --
    -- -   @no-override@ – If the viewer request doesn\'t contain the
    --     @Authorization@ header, then CloudFront signs the origin request. If
    --     the viewer request contains the @Authorization@ header, then
    --     CloudFront doesn\'t sign the origin request and instead passes along
    --     the @Authorization@ header from the viewer request. __WARNING: To
    --     pass along the @Authorization@ header from the viewer request, you
    --     /must/ add the @Authorization@ header to a
    --     <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html cache policy>
    --     for all cache behaviors that use origins associated with this origin
    --     access control.__
    signingBehavior :: OriginAccessControlSigningBehaviors,
    -- | The type of origin that this origin access control is for. The only
    -- valid value is @s3@.
    originAccessControlOriginType :: OriginAccessControlOriginTypes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginAccessControlConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'originAccessControlConfig_description' - A description of the origin access control.
--
-- 'name', 'originAccessControlConfig_name' - A name to identify the origin access control.
--
-- 'signingProtocol', 'originAccessControlConfig_signingProtocol' - The signing protocol of the origin access control, which determines how
-- CloudFront signs (authenticates) requests. The only valid value is
-- @sigv4@.
--
-- 'signingBehavior', 'originAccessControlConfig_signingBehavior' - Specifies which requests CloudFront signs (adds authentication
-- information to). Specify @always@ for the most common use case. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html#oac-advanced-settings origin access control advanced settings>
-- in the /Amazon CloudFront Developer Guide/.
--
-- This field can have one of the following values:
--
-- -   @always@ – CloudFront signs all origin requests, overwriting the
--     @Authorization@ header from the viewer request if one exists.
--
-- -   @never@ – CloudFront doesn\'t sign any origin requests. This value
--     turns off origin access control for all origins in all distributions
--     that use this origin access control.
--
-- -   @no-override@ – If the viewer request doesn\'t contain the
--     @Authorization@ header, then CloudFront signs the origin request. If
--     the viewer request contains the @Authorization@ header, then
--     CloudFront doesn\'t sign the origin request and instead passes along
--     the @Authorization@ header from the viewer request. __WARNING: To
--     pass along the @Authorization@ header from the viewer request, you
--     /must/ add the @Authorization@ header to a
--     <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html cache policy>
--     for all cache behaviors that use origins associated with this origin
--     access control.__
--
-- 'originAccessControlOriginType', 'originAccessControlConfig_originAccessControlOriginType' - The type of origin that this origin access control is for. The only
-- valid value is @s3@.
newOriginAccessControlConfig ::
  -- | 'name'
  Prelude.Text ->
  -- | 'signingProtocol'
  OriginAccessControlSigningProtocols ->
  -- | 'signingBehavior'
  OriginAccessControlSigningBehaviors ->
  -- | 'originAccessControlOriginType'
  OriginAccessControlOriginTypes ->
  OriginAccessControlConfig
newOriginAccessControlConfig
  pName_
  pSigningProtocol_
  pSigningBehavior_
  pOriginAccessControlOriginType_ =
    OriginAccessControlConfig'
      { description =
          Prelude.Nothing,
        name = pName_,
        signingProtocol = pSigningProtocol_,
        signingBehavior = pSigningBehavior_,
        originAccessControlOriginType =
          pOriginAccessControlOriginType_
      }

-- | A description of the origin access control.
originAccessControlConfig_description :: Lens.Lens' OriginAccessControlConfig (Prelude.Maybe Prelude.Text)
originAccessControlConfig_description = Lens.lens (\OriginAccessControlConfig' {description} -> description) (\s@OriginAccessControlConfig' {} a -> s {description = a} :: OriginAccessControlConfig)

-- | A name to identify the origin access control.
originAccessControlConfig_name :: Lens.Lens' OriginAccessControlConfig Prelude.Text
originAccessControlConfig_name = Lens.lens (\OriginAccessControlConfig' {name} -> name) (\s@OriginAccessControlConfig' {} a -> s {name = a} :: OriginAccessControlConfig)

-- | The signing protocol of the origin access control, which determines how
-- CloudFront signs (authenticates) requests. The only valid value is
-- @sigv4@.
originAccessControlConfig_signingProtocol :: Lens.Lens' OriginAccessControlConfig OriginAccessControlSigningProtocols
originAccessControlConfig_signingProtocol = Lens.lens (\OriginAccessControlConfig' {signingProtocol} -> signingProtocol) (\s@OriginAccessControlConfig' {} a -> s {signingProtocol = a} :: OriginAccessControlConfig)

-- | Specifies which requests CloudFront signs (adds authentication
-- information to). Specify @always@ for the most common use case. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html#oac-advanced-settings origin access control advanced settings>
-- in the /Amazon CloudFront Developer Guide/.
--
-- This field can have one of the following values:
--
-- -   @always@ – CloudFront signs all origin requests, overwriting the
--     @Authorization@ header from the viewer request if one exists.
--
-- -   @never@ – CloudFront doesn\'t sign any origin requests. This value
--     turns off origin access control for all origins in all distributions
--     that use this origin access control.
--
-- -   @no-override@ – If the viewer request doesn\'t contain the
--     @Authorization@ header, then CloudFront signs the origin request. If
--     the viewer request contains the @Authorization@ header, then
--     CloudFront doesn\'t sign the origin request and instead passes along
--     the @Authorization@ header from the viewer request. __WARNING: To
--     pass along the @Authorization@ header from the viewer request, you
--     /must/ add the @Authorization@ header to a
--     <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html cache policy>
--     for all cache behaviors that use origins associated with this origin
--     access control.__
originAccessControlConfig_signingBehavior :: Lens.Lens' OriginAccessControlConfig OriginAccessControlSigningBehaviors
originAccessControlConfig_signingBehavior = Lens.lens (\OriginAccessControlConfig' {signingBehavior} -> signingBehavior) (\s@OriginAccessControlConfig' {} a -> s {signingBehavior = a} :: OriginAccessControlConfig)

-- | The type of origin that this origin access control is for. The only
-- valid value is @s3@.
originAccessControlConfig_originAccessControlOriginType :: Lens.Lens' OriginAccessControlConfig OriginAccessControlOriginTypes
originAccessControlConfig_originAccessControlOriginType = Lens.lens (\OriginAccessControlConfig' {originAccessControlOriginType} -> originAccessControlOriginType) (\s@OriginAccessControlConfig' {} a -> s {originAccessControlOriginType = a} :: OriginAccessControlConfig)

instance Core.FromXML OriginAccessControlConfig where
  parseXML x =
    OriginAccessControlConfig'
      Prelude.<$> (x Core..@? "Description")
      Prelude.<*> (x Core..@ "Name")
      Prelude.<*> (x Core..@ "SigningProtocol")
      Prelude.<*> (x Core..@ "SigningBehavior")
      Prelude.<*> (x Core..@ "OriginAccessControlOriginType")

instance Prelude.Hashable OriginAccessControlConfig where
  hashWithSalt _salt OriginAccessControlConfig' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` signingProtocol
      `Prelude.hashWithSalt` signingBehavior
      `Prelude.hashWithSalt` originAccessControlOriginType

instance Prelude.NFData OriginAccessControlConfig where
  rnf OriginAccessControlConfig' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf signingProtocol
      `Prelude.seq` Prelude.rnf signingBehavior
      `Prelude.seq` Prelude.rnf originAccessControlOriginType

instance Core.ToXML OriginAccessControlConfig where
  toXML OriginAccessControlConfig' {..} =
    Prelude.mconcat
      [ "Description" Core.@= description,
        "Name" Core.@= name,
        "SigningProtocol" Core.@= signingProtocol,
        "SigningBehavior" Core.@= signingBehavior,
        "OriginAccessControlOriginType"
          Core.@= originAccessControlOriginType
      ]
