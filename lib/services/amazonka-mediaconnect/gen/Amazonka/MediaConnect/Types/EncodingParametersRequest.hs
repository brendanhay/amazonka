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
-- Module      : Amazonka.MediaConnect.Types.EncodingParametersRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.EncodingParametersRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.EncoderProfile
import qualified Amazonka.Prelude as Prelude

-- | A collection of parameters that determine how MediaConnect will convert
-- the content. These fields only apply to outputs on flows that have a CDI
-- source.
--
-- /See:/ 'newEncodingParametersRequest' smart constructor.
data EncodingParametersRequest = EncodingParametersRequest'
  { -- | A setting on the encoder that drives compression settings. This property
    -- only applies to video media streams associated with outputs that use the
    -- ST 2110 JPEG XS protocol, if at least one source on the flow uses the
    -- CDI protocol.
    encoderProfile :: EncoderProfile,
    -- | A value that is used to calculate compression for an output. The bitrate
    -- of the output is calculated as follows: Output bitrate = (1 \/
    -- compressionFactor) * (source bitrate) This property only applies to
    -- outputs that use the ST 2110 JPEG XS protocol, with a flow source that
    -- uses the CDI protocol. Valid values are floating point numbers in the
    -- range of 3.0 to 10.0, inclusive.
    compressionFactor :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncodingParametersRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encoderProfile', 'encodingParametersRequest_encoderProfile' - A setting on the encoder that drives compression settings. This property
-- only applies to video media streams associated with outputs that use the
-- ST 2110 JPEG XS protocol, if at least one source on the flow uses the
-- CDI protocol.
--
-- 'compressionFactor', 'encodingParametersRequest_compressionFactor' - A value that is used to calculate compression for an output. The bitrate
-- of the output is calculated as follows: Output bitrate = (1 \/
-- compressionFactor) * (source bitrate) This property only applies to
-- outputs that use the ST 2110 JPEG XS protocol, with a flow source that
-- uses the CDI protocol. Valid values are floating point numbers in the
-- range of 3.0 to 10.0, inclusive.
newEncodingParametersRequest ::
  -- | 'encoderProfile'
  EncoderProfile ->
  -- | 'compressionFactor'
  Prelude.Double ->
  EncodingParametersRequest
newEncodingParametersRequest
  pEncoderProfile_
  pCompressionFactor_ =
    EncodingParametersRequest'
      { encoderProfile =
          pEncoderProfile_,
        compressionFactor = pCompressionFactor_
      }

-- | A setting on the encoder that drives compression settings. This property
-- only applies to video media streams associated with outputs that use the
-- ST 2110 JPEG XS protocol, if at least one source on the flow uses the
-- CDI protocol.
encodingParametersRequest_encoderProfile :: Lens.Lens' EncodingParametersRequest EncoderProfile
encodingParametersRequest_encoderProfile = Lens.lens (\EncodingParametersRequest' {encoderProfile} -> encoderProfile) (\s@EncodingParametersRequest' {} a -> s {encoderProfile = a} :: EncodingParametersRequest)

-- | A value that is used to calculate compression for an output. The bitrate
-- of the output is calculated as follows: Output bitrate = (1 \/
-- compressionFactor) * (source bitrate) This property only applies to
-- outputs that use the ST 2110 JPEG XS protocol, with a flow source that
-- uses the CDI protocol. Valid values are floating point numbers in the
-- range of 3.0 to 10.0, inclusive.
encodingParametersRequest_compressionFactor :: Lens.Lens' EncodingParametersRequest Prelude.Double
encodingParametersRequest_compressionFactor = Lens.lens (\EncodingParametersRequest' {compressionFactor} -> compressionFactor) (\s@EncodingParametersRequest' {} a -> s {compressionFactor = a} :: EncodingParametersRequest)

instance Prelude.Hashable EncodingParametersRequest where
  hashWithSalt _salt EncodingParametersRequest' {..} =
    _salt `Prelude.hashWithSalt` encoderProfile
      `Prelude.hashWithSalt` compressionFactor

instance Prelude.NFData EncodingParametersRequest where
  rnf EncodingParametersRequest' {..} =
    Prelude.rnf encoderProfile
      `Prelude.seq` Prelude.rnf compressionFactor

instance Data.ToJSON EncodingParametersRequest where
  toJSON EncodingParametersRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("encoderProfile" Data..= encoderProfile),
            Prelude.Just
              ("compressionFactor" Data..= compressionFactor)
          ]
      )
