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
-- Module      : Amazonka.MediaConvert.Types.Policy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Policy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.InputPolicy
import qualified Amazonka.Prelude as Prelude

-- | A policy configures behavior that you allow or disallow for your
-- account. For information about MediaConvert policies, see the user guide
-- at http:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
--
-- /See:/ 'newPolicy' smart constructor.
data Policy = Policy'
  { -- | Allow or disallow jobs that specify HTTP inputs.
    httpInputs :: Prelude.Maybe InputPolicy,
    -- | Allow or disallow jobs that specify HTTPS inputs.
    httpsInputs :: Prelude.Maybe InputPolicy,
    -- | Allow or disallow jobs that specify Amazon S3 inputs.
    s3Inputs :: Prelude.Maybe InputPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Policy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpInputs', 'policy_httpInputs' - Allow or disallow jobs that specify HTTP inputs.
--
-- 'httpsInputs', 'policy_httpsInputs' - Allow or disallow jobs that specify HTTPS inputs.
--
-- 's3Inputs', 'policy_s3Inputs' - Allow or disallow jobs that specify Amazon S3 inputs.
newPolicy ::
  Policy
newPolicy =
  Policy'
    { httpInputs = Prelude.Nothing,
      httpsInputs = Prelude.Nothing,
      s3Inputs = Prelude.Nothing
    }

-- | Allow or disallow jobs that specify HTTP inputs.
policy_httpInputs :: Lens.Lens' Policy (Prelude.Maybe InputPolicy)
policy_httpInputs = Lens.lens (\Policy' {httpInputs} -> httpInputs) (\s@Policy' {} a -> s {httpInputs = a} :: Policy)

-- | Allow or disallow jobs that specify HTTPS inputs.
policy_httpsInputs :: Lens.Lens' Policy (Prelude.Maybe InputPolicy)
policy_httpsInputs = Lens.lens (\Policy' {httpsInputs} -> httpsInputs) (\s@Policy' {} a -> s {httpsInputs = a} :: Policy)

-- | Allow or disallow jobs that specify Amazon S3 inputs.
policy_s3Inputs :: Lens.Lens' Policy (Prelude.Maybe InputPolicy)
policy_s3Inputs = Lens.lens (\Policy' {s3Inputs} -> s3Inputs) (\s@Policy' {} a -> s {s3Inputs = a} :: Policy)

instance Data.FromJSON Policy where
  parseJSON =
    Data.withObject
      "Policy"
      ( \x ->
          Policy'
            Prelude.<$> (x Data..:? "httpInputs")
            Prelude.<*> (x Data..:? "httpsInputs")
            Prelude.<*> (x Data..:? "s3Inputs")
      )

instance Prelude.Hashable Policy where
  hashWithSalt _salt Policy' {..} =
    _salt
      `Prelude.hashWithSalt` httpInputs
      `Prelude.hashWithSalt` httpsInputs
      `Prelude.hashWithSalt` s3Inputs

instance Prelude.NFData Policy where
  rnf Policy' {..} =
    Prelude.rnf httpInputs `Prelude.seq`
      Prelude.rnf httpsInputs `Prelude.seq`
        Prelude.rnf s3Inputs

instance Data.ToJSON Policy where
  toJSON Policy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("httpInputs" Data..=) Prelude.<$> httpInputs,
            ("httpsInputs" Data..=) Prelude.<$> httpsInputs,
            ("s3Inputs" Data..=) Prelude.<$> s3Inputs
          ]
      )
