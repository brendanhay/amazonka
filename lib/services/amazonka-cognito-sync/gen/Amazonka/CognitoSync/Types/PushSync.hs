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
-- Module      : Amazonka.CognitoSync.Types.PushSync
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoSync.Types.PushSync where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration options to be applied to the identity pool.
--
-- /See:/ 'newPushSync' smart constructor.
data PushSync = PushSync'
  { -- | List of SNS platform application ARNs that could be used by clients.
    applicationArns :: Prelude.Maybe [Prelude.Text],
    -- | A role configured to allow Cognito to call SNS on behalf of the
    -- developer.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PushSync' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationArns', 'pushSync_applicationArns' - List of SNS platform application ARNs that could be used by clients.
--
-- 'roleArn', 'pushSync_roleArn' - A role configured to allow Cognito to call SNS on behalf of the
-- developer.
newPushSync ::
  PushSync
newPushSync =
  PushSync'
    { applicationArns = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | List of SNS platform application ARNs that could be used by clients.
pushSync_applicationArns :: Lens.Lens' PushSync (Prelude.Maybe [Prelude.Text])
pushSync_applicationArns = Lens.lens (\PushSync' {applicationArns} -> applicationArns) (\s@PushSync' {} a -> s {applicationArns = a} :: PushSync) Prelude.. Lens.mapping Lens.coerced

-- | A role configured to allow Cognito to call SNS on behalf of the
-- developer.
pushSync_roleArn :: Lens.Lens' PushSync (Prelude.Maybe Prelude.Text)
pushSync_roleArn = Lens.lens (\PushSync' {roleArn} -> roleArn) (\s@PushSync' {} a -> s {roleArn = a} :: PushSync)

instance Data.FromJSON PushSync where
  parseJSON =
    Data.withObject
      "PushSync"
      ( \x ->
          PushSync'
            Prelude.<$> ( x
                            Data..:? "ApplicationArns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "RoleArn")
      )

instance Prelude.Hashable PushSync where
  hashWithSalt _salt PushSync' {..} =
    _salt
      `Prelude.hashWithSalt` applicationArns
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData PushSync where
  rnf PushSync' {..} =
    Prelude.rnf applicationArns
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON PushSync where
  toJSON PushSync' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplicationArns" Data..=)
              Prelude.<$> applicationArns,
            ("RoleArn" Data..=) Prelude.<$> roleArn
          ]
      )
