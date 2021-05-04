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
-- Module      : Network.AWS.SageMaker.Types.UserContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UserContext where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the user who created or modified an experiment, trial,
-- or trial component.
--
-- /See:/ 'newUserContext' smart constructor.
data UserContext = UserContext'
  { -- | The name of the user\'s profile.
    userProfileName :: Prelude.Maybe Prelude.Text,
    -- | The domain associated with the user.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user\'s profile.
    userProfileArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UserContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userProfileName', 'userContext_userProfileName' - The name of the user\'s profile.
--
-- 'domainId', 'userContext_domainId' - The domain associated with the user.
--
-- 'userProfileArn', 'userContext_userProfileArn' - The Amazon Resource Name (ARN) of the user\'s profile.
newUserContext ::
  UserContext
newUserContext =
  UserContext'
    { userProfileName = Prelude.Nothing,
      domainId = Prelude.Nothing,
      userProfileArn = Prelude.Nothing
    }

-- | The name of the user\'s profile.
userContext_userProfileName :: Lens.Lens' UserContext (Prelude.Maybe Prelude.Text)
userContext_userProfileName = Lens.lens (\UserContext' {userProfileName} -> userProfileName) (\s@UserContext' {} a -> s {userProfileName = a} :: UserContext)

-- | The domain associated with the user.
userContext_domainId :: Lens.Lens' UserContext (Prelude.Maybe Prelude.Text)
userContext_domainId = Lens.lens (\UserContext' {domainId} -> domainId) (\s@UserContext' {} a -> s {domainId = a} :: UserContext)

-- | The Amazon Resource Name (ARN) of the user\'s profile.
userContext_userProfileArn :: Lens.Lens' UserContext (Prelude.Maybe Prelude.Text)
userContext_userProfileArn = Lens.lens (\UserContext' {userProfileArn} -> userProfileArn) (\s@UserContext' {} a -> s {userProfileArn = a} :: UserContext)

instance Prelude.FromJSON UserContext where
  parseJSON =
    Prelude.withObject
      "UserContext"
      ( \x ->
          UserContext'
            Prelude.<$> (x Prelude..:? "UserProfileName")
            Prelude.<*> (x Prelude..:? "DomainId")
            Prelude.<*> (x Prelude..:? "UserProfileArn")
      )

instance Prelude.Hashable UserContext

instance Prelude.NFData UserContext
