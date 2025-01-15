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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.OriginationIdentityMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.OriginationIdentityMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types.NumberCapability
import qualified Amazonka.Prelude as Prelude

-- | The metadata for an origination identity associated with a pool.
--
-- /See:/ 'newOriginationIdentityMetadata' smart constructor.
data OriginationIdentityMetadata = OriginationIdentityMetadata'
  { -- | The Amazon Resource Name (ARN) associated with the origination identity.
    originationIdentityArn :: Prelude.Text,
    -- | The unique identifier of the origination identity.
    originationIdentity :: Prelude.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region.
    isoCountryCode :: Prelude.Text,
    -- | Describes if the origination identity can be used for text messages,
    -- voice calls or both.
    numberCapabilities :: Prelude.NonEmpty NumberCapability
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginationIdentityMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originationIdentityArn', 'originationIdentityMetadata_originationIdentityArn' - The Amazon Resource Name (ARN) associated with the origination identity.
--
-- 'originationIdentity', 'originationIdentityMetadata_originationIdentity' - The unique identifier of the origination identity.
--
-- 'isoCountryCode', 'originationIdentityMetadata_isoCountryCode' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
--
-- 'numberCapabilities', 'originationIdentityMetadata_numberCapabilities' - Describes if the origination identity can be used for text messages,
-- voice calls or both.
newOriginationIdentityMetadata ::
  -- | 'originationIdentityArn'
  Prelude.Text ->
  -- | 'originationIdentity'
  Prelude.Text ->
  -- | 'isoCountryCode'
  Prelude.Text ->
  -- | 'numberCapabilities'
  Prelude.NonEmpty NumberCapability ->
  OriginationIdentityMetadata
newOriginationIdentityMetadata
  pOriginationIdentityArn_
  pOriginationIdentity_
  pIsoCountryCode_
  pNumberCapabilities_ =
    OriginationIdentityMetadata'
      { originationIdentityArn =
          pOriginationIdentityArn_,
        originationIdentity = pOriginationIdentity_,
        isoCountryCode = pIsoCountryCode_,
        numberCapabilities =
          Lens.coerced Lens.# pNumberCapabilities_
      }

-- | The Amazon Resource Name (ARN) associated with the origination identity.
originationIdentityMetadata_originationIdentityArn :: Lens.Lens' OriginationIdentityMetadata Prelude.Text
originationIdentityMetadata_originationIdentityArn = Lens.lens (\OriginationIdentityMetadata' {originationIdentityArn} -> originationIdentityArn) (\s@OriginationIdentityMetadata' {} a -> s {originationIdentityArn = a} :: OriginationIdentityMetadata)

-- | The unique identifier of the origination identity.
originationIdentityMetadata_originationIdentity :: Lens.Lens' OriginationIdentityMetadata Prelude.Text
originationIdentityMetadata_originationIdentity = Lens.lens (\OriginationIdentityMetadata' {originationIdentity} -> originationIdentity) (\s@OriginationIdentityMetadata' {} a -> s {originationIdentity = a} :: OriginationIdentityMetadata)

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
originationIdentityMetadata_isoCountryCode :: Lens.Lens' OriginationIdentityMetadata Prelude.Text
originationIdentityMetadata_isoCountryCode = Lens.lens (\OriginationIdentityMetadata' {isoCountryCode} -> isoCountryCode) (\s@OriginationIdentityMetadata' {} a -> s {isoCountryCode = a} :: OriginationIdentityMetadata)

-- | Describes if the origination identity can be used for text messages,
-- voice calls or both.
originationIdentityMetadata_numberCapabilities :: Lens.Lens' OriginationIdentityMetadata (Prelude.NonEmpty NumberCapability)
originationIdentityMetadata_numberCapabilities = Lens.lens (\OriginationIdentityMetadata' {numberCapabilities} -> numberCapabilities) (\s@OriginationIdentityMetadata' {} a -> s {numberCapabilities = a} :: OriginationIdentityMetadata) Prelude.. Lens.coerced

instance Data.FromJSON OriginationIdentityMetadata where
  parseJSON =
    Data.withObject
      "OriginationIdentityMetadata"
      ( \x ->
          OriginationIdentityMetadata'
            Prelude.<$> (x Data..: "OriginationIdentityArn")
            Prelude.<*> (x Data..: "OriginationIdentity")
            Prelude.<*> (x Data..: "IsoCountryCode")
            Prelude.<*> (x Data..: "NumberCapabilities")
      )

instance Prelude.Hashable OriginationIdentityMetadata where
  hashWithSalt _salt OriginationIdentityMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` originationIdentityArn
      `Prelude.hashWithSalt` originationIdentity
      `Prelude.hashWithSalt` isoCountryCode
      `Prelude.hashWithSalt` numberCapabilities

instance Prelude.NFData OriginationIdentityMetadata where
  rnf OriginationIdentityMetadata' {..} =
    Prelude.rnf originationIdentityArn `Prelude.seq`
      Prelude.rnf originationIdentity `Prelude.seq`
        Prelude.rnf isoCountryCode `Prelude.seq`
          Prelude.rnf numberCapabilities
