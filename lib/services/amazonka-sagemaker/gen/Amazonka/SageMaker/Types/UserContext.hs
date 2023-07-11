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
-- Module      : Amazonka.SageMaker.Types.UserContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.UserContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the user who created or modified an experiment, trial,
-- trial component, lineage group, project, or model card.
--
-- /See:/ 'newUserContext' smart constructor.
data UserContext = UserContext'
  { -- | The domain associated with the user.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user\'s profile.
    userProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the user\'s profile.
    userProfileName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'userContext_domainId' - The domain associated with the user.
--
-- 'userProfileArn', 'userContext_userProfileArn' - The Amazon Resource Name (ARN) of the user\'s profile.
--
-- 'userProfileName', 'userContext_userProfileName' - The name of the user\'s profile.
newUserContext ::
  UserContext
newUserContext =
  UserContext'
    { domainId = Prelude.Nothing,
      userProfileArn = Prelude.Nothing,
      userProfileName = Prelude.Nothing
    }

-- | The domain associated with the user.
userContext_domainId :: Lens.Lens' UserContext (Prelude.Maybe Prelude.Text)
userContext_domainId = Lens.lens (\UserContext' {domainId} -> domainId) (\s@UserContext' {} a -> s {domainId = a} :: UserContext)

-- | The Amazon Resource Name (ARN) of the user\'s profile.
userContext_userProfileArn :: Lens.Lens' UserContext (Prelude.Maybe Prelude.Text)
userContext_userProfileArn = Lens.lens (\UserContext' {userProfileArn} -> userProfileArn) (\s@UserContext' {} a -> s {userProfileArn = a} :: UserContext)

-- | The name of the user\'s profile.
userContext_userProfileName :: Lens.Lens' UserContext (Prelude.Maybe Prelude.Text)
userContext_userProfileName = Lens.lens (\UserContext' {userProfileName} -> userProfileName) (\s@UserContext' {} a -> s {userProfileName = a} :: UserContext)

instance Data.FromJSON UserContext where
  parseJSON =
    Data.withObject
      "UserContext"
      ( \x ->
          UserContext'
            Prelude.<$> (x Data..:? "DomainId")
            Prelude.<*> (x Data..:? "UserProfileArn")
            Prelude.<*> (x Data..:? "UserProfileName")
      )

instance Prelude.Hashable UserContext where
  hashWithSalt _salt UserContext' {..} =
    _salt
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` userProfileArn
      `Prelude.hashWithSalt` userProfileName

instance Prelude.NFData UserContext where
  rnf UserContext' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf userProfileArn
      `Prelude.seq` Prelude.rnf userProfileName
