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
-- Module      : Amazonka.CloudFront.Types.OriginAccessControlSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginAccessControlSummary where

import Amazonka.CloudFront.Types.OriginAccessControlOriginTypes
import Amazonka.CloudFront.Types.OriginAccessControlSigningBehaviors
import Amazonka.CloudFront.Types.OriginAccessControlSigningProtocols
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A CloudFront origin access control.
--
-- /See:/ 'newOriginAccessControlSummary' smart constructor.
data OriginAccessControlSummary = OriginAccessControlSummary'
  { -- | The unique identifier of the origin access control.
    id :: Prelude.Text,
    -- | A description of the origin access control.
    description :: Prelude.Text,
    -- | A unique name that identifies the origin access control.
    name :: Prelude.Text,
    -- | The signing protocol of the origin access control. The signing protocol
    -- determines how CloudFront signs (authenticates) requests. The only valid
    -- value is @sigv4@.
    signingProtocol :: OriginAccessControlSigningProtocols,
    -- | A value that specifies which requests CloudFront signs (adds
    -- authentication information to). This field can have one of the following
    -- values:
    --
    -- -   @never@ – CloudFront doesn\'t sign any origin requests.
    --
    -- -   @always@ – CloudFront signs all origin requests, overwriting the
    --     @Authorization@ header from the viewer request if necessary.
    --
    -- -   @no-override@ – If the viewer request doesn\'t contain the
    --     @Authorization@ header, CloudFront signs the origin request. If the
    --     viewer request contains the @Authorization@ header, CloudFront
    --     doesn\'t sign the origin request, but instead passes along the
    --     @Authorization@ header that it received in the viewer request.
    signingBehavior :: OriginAccessControlSigningBehaviors,
    -- | The type of origin that this origin access control is for. The only
    -- valid value is @s3@.
    originAccessControlOriginType :: OriginAccessControlOriginTypes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginAccessControlSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'originAccessControlSummary_id' - The unique identifier of the origin access control.
--
-- 'description', 'originAccessControlSummary_description' - A description of the origin access control.
--
-- 'name', 'originAccessControlSummary_name' - A unique name that identifies the origin access control.
--
-- 'signingProtocol', 'originAccessControlSummary_signingProtocol' - The signing protocol of the origin access control. The signing protocol
-- determines how CloudFront signs (authenticates) requests. The only valid
-- value is @sigv4@.
--
-- 'signingBehavior', 'originAccessControlSummary_signingBehavior' - A value that specifies which requests CloudFront signs (adds
-- authentication information to). This field can have one of the following
-- values:
--
-- -   @never@ – CloudFront doesn\'t sign any origin requests.
--
-- -   @always@ – CloudFront signs all origin requests, overwriting the
--     @Authorization@ header from the viewer request if necessary.
--
-- -   @no-override@ – If the viewer request doesn\'t contain the
--     @Authorization@ header, CloudFront signs the origin request. If the
--     viewer request contains the @Authorization@ header, CloudFront
--     doesn\'t sign the origin request, but instead passes along the
--     @Authorization@ header that it received in the viewer request.
--
-- 'originAccessControlOriginType', 'originAccessControlSummary_originAccessControlOriginType' - The type of origin that this origin access control is for. The only
-- valid value is @s3@.
newOriginAccessControlSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'signingProtocol'
  OriginAccessControlSigningProtocols ->
  -- | 'signingBehavior'
  OriginAccessControlSigningBehaviors ->
  -- | 'originAccessControlOriginType'
  OriginAccessControlOriginTypes ->
  OriginAccessControlSummary
newOriginAccessControlSummary
  pId_
  pDescription_
  pName_
  pSigningProtocol_
  pSigningBehavior_
  pOriginAccessControlOriginType_ =
    OriginAccessControlSummary'
      { id = pId_,
        description = pDescription_,
        name = pName_,
        signingProtocol = pSigningProtocol_,
        signingBehavior = pSigningBehavior_,
        originAccessControlOriginType =
          pOriginAccessControlOriginType_
      }

-- | The unique identifier of the origin access control.
originAccessControlSummary_id :: Lens.Lens' OriginAccessControlSummary Prelude.Text
originAccessControlSummary_id = Lens.lens (\OriginAccessControlSummary' {id} -> id) (\s@OriginAccessControlSummary' {} a -> s {id = a} :: OriginAccessControlSummary)

-- | A description of the origin access control.
originAccessControlSummary_description :: Lens.Lens' OriginAccessControlSummary Prelude.Text
originAccessControlSummary_description = Lens.lens (\OriginAccessControlSummary' {description} -> description) (\s@OriginAccessControlSummary' {} a -> s {description = a} :: OriginAccessControlSummary)

-- | A unique name that identifies the origin access control.
originAccessControlSummary_name :: Lens.Lens' OriginAccessControlSummary Prelude.Text
originAccessControlSummary_name = Lens.lens (\OriginAccessControlSummary' {name} -> name) (\s@OriginAccessControlSummary' {} a -> s {name = a} :: OriginAccessControlSummary)

-- | The signing protocol of the origin access control. The signing protocol
-- determines how CloudFront signs (authenticates) requests. The only valid
-- value is @sigv4@.
originAccessControlSummary_signingProtocol :: Lens.Lens' OriginAccessControlSummary OriginAccessControlSigningProtocols
originAccessControlSummary_signingProtocol = Lens.lens (\OriginAccessControlSummary' {signingProtocol} -> signingProtocol) (\s@OriginAccessControlSummary' {} a -> s {signingProtocol = a} :: OriginAccessControlSummary)

-- | A value that specifies which requests CloudFront signs (adds
-- authentication information to). This field can have one of the following
-- values:
--
-- -   @never@ – CloudFront doesn\'t sign any origin requests.
--
-- -   @always@ – CloudFront signs all origin requests, overwriting the
--     @Authorization@ header from the viewer request if necessary.
--
-- -   @no-override@ – If the viewer request doesn\'t contain the
--     @Authorization@ header, CloudFront signs the origin request. If the
--     viewer request contains the @Authorization@ header, CloudFront
--     doesn\'t sign the origin request, but instead passes along the
--     @Authorization@ header that it received in the viewer request.
originAccessControlSummary_signingBehavior :: Lens.Lens' OriginAccessControlSummary OriginAccessControlSigningBehaviors
originAccessControlSummary_signingBehavior = Lens.lens (\OriginAccessControlSummary' {signingBehavior} -> signingBehavior) (\s@OriginAccessControlSummary' {} a -> s {signingBehavior = a} :: OriginAccessControlSummary)

-- | The type of origin that this origin access control is for. The only
-- valid value is @s3@.
originAccessControlSummary_originAccessControlOriginType :: Lens.Lens' OriginAccessControlSummary OriginAccessControlOriginTypes
originAccessControlSummary_originAccessControlOriginType = Lens.lens (\OriginAccessControlSummary' {originAccessControlOriginType} -> originAccessControlOriginType) (\s@OriginAccessControlSummary' {} a -> s {originAccessControlOriginType = a} :: OriginAccessControlSummary)

instance Data.FromXML OriginAccessControlSummary where
  parseXML x =
    OriginAccessControlSummary'
      Prelude.<$> (x Data..@ "Id")
      Prelude.<*> (x Data..@ "Description")
      Prelude.<*> (x Data..@ "Name")
      Prelude.<*> (x Data..@ "SigningProtocol")
      Prelude.<*> (x Data..@ "SigningBehavior")
      Prelude.<*> (x Data..@ "OriginAccessControlOriginType")

instance Prelude.Hashable OriginAccessControlSummary where
  hashWithSalt _salt OriginAccessControlSummary' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` signingProtocol
      `Prelude.hashWithSalt` signingBehavior
      `Prelude.hashWithSalt` originAccessControlOriginType

instance Prelude.NFData OriginAccessControlSummary where
  rnf OriginAccessControlSummary' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf signingProtocol `Prelude.seq`
            Prelude.rnf signingBehavior `Prelude.seq`
              Prelude.rnf originAccessControlOriginType
