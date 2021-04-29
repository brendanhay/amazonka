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
-- Module      : Network.AWS.CognitoSync.Types.PushSync
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.PushSync where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration options to be applied to the identity pool.
--
-- /See:/ 'newPushSync' smart constructor.
data PushSync = PushSync'
  { -- | A role configured to allow Cognito to call SNS on behalf of the
    -- developer.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | List of SNS platform application ARNs that could be used by clients.
    applicationArns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PushSync' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'pushSync_roleArn' - A role configured to allow Cognito to call SNS on behalf of the
-- developer.
--
-- 'applicationArns', 'pushSync_applicationArns' - List of SNS platform application ARNs that could be used by clients.
newPushSync ::
  PushSync
newPushSync =
  PushSync'
    { roleArn = Prelude.Nothing,
      applicationArns = Prelude.Nothing
    }

-- | A role configured to allow Cognito to call SNS on behalf of the
-- developer.
pushSync_roleArn :: Lens.Lens' PushSync (Prelude.Maybe Prelude.Text)
pushSync_roleArn = Lens.lens (\PushSync' {roleArn} -> roleArn) (\s@PushSync' {} a -> s {roleArn = a} :: PushSync)

-- | List of SNS platform application ARNs that could be used by clients.
pushSync_applicationArns :: Lens.Lens' PushSync (Prelude.Maybe [Prelude.Text])
pushSync_applicationArns = Lens.lens (\PushSync' {applicationArns} -> applicationArns) (\s@PushSync' {} a -> s {applicationArns = a} :: PushSync) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON PushSync where
  parseJSON =
    Prelude.withObject
      "PushSync"
      ( \x ->
          PushSync'
            Prelude.<$> (x Prelude..:? "RoleArn")
            Prelude.<*> ( x Prelude..:? "ApplicationArns"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PushSync

instance Prelude.NFData PushSync

instance Prelude.ToJSON PushSync where
  toJSON PushSync' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RoleArn" Prelude..=) Prelude.<$> roleArn,
            ("ApplicationArns" Prelude..=)
              Prelude.<$> applicationArns
          ]
      )
