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
-- Module      : Amazonka.Chime.Types.SipMediaApplicationEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.SipMediaApplicationEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The endpoint assigned to the SIP media application.
--
-- /See:/ 'newSipMediaApplicationEndpoint' smart constructor.
data SipMediaApplicationEndpoint = SipMediaApplicationEndpoint'
  { -- | Valid Amazon Resource Name (ARN) of the Lambda function, version, or
    -- alias. The function must be created in the same AWS Region as the SIP
    -- media application.
    lambdaArn :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SipMediaApplicationEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaArn', 'sipMediaApplicationEndpoint_lambdaArn' - Valid Amazon Resource Name (ARN) of the Lambda function, version, or
-- alias. The function must be created in the same AWS Region as the SIP
-- media application.
newSipMediaApplicationEndpoint ::
  SipMediaApplicationEndpoint
newSipMediaApplicationEndpoint =
  SipMediaApplicationEndpoint'
    { lambdaArn =
        Prelude.Nothing
    }

-- | Valid Amazon Resource Name (ARN) of the Lambda function, version, or
-- alias. The function must be created in the same AWS Region as the SIP
-- media application.
sipMediaApplicationEndpoint_lambdaArn :: Lens.Lens' SipMediaApplicationEndpoint (Prelude.Maybe Prelude.Text)
sipMediaApplicationEndpoint_lambdaArn = Lens.lens (\SipMediaApplicationEndpoint' {lambdaArn} -> lambdaArn) (\s@SipMediaApplicationEndpoint' {} a -> s {lambdaArn = a} :: SipMediaApplicationEndpoint) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON SipMediaApplicationEndpoint where
  parseJSON =
    Data.withObject
      "SipMediaApplicationEndpoint"
      ( \x ->
          SipMediaApplicationEndpoint'
            Prelude.<$> (x Data..:? "LambdaArn")
      )

instance Prelude.Hashable SipMediaApplicationEndpoint where
  hashWithSalt _salt SipMediaApplicationEndpoint' {..} =
    _salt `Prelude.hashWithSalt` lambdaArn

instance Prelude.NFData SipMediaApplicationEndpoint where
  rnf SipMediaApplicationEndpoint' {..} =
    Prelude.rnf lambdaArn

instance Data.ToJSON SipMediaApplicationEndpoint where
  toJSON SipMediaApplicationEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [("LambdaArn" Data..=) Prelude.<$> lambdaArn]
      )
