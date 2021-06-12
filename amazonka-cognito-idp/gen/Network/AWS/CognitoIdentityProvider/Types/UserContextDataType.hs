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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserContextDataType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserContextDataType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
--
-- /See:/ 'newUserContextDataType' smart constructor.
data UserContextDataType = UserContextDataType'
  { -- | Contextual data such as the user\'s device fingerprint, IP address, or
    -- location used for evaluating the risk of an unexpected event by Amazon
    -- Cognito advanced security.
    encodedData :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserContextDataType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encodedData', 'userContextDataType_encodedData' - Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
newUserContextDataType ::
  UserContextDataType
newUserContextDataType =
  UserContextDataType' {encodedData = Core.Nothing}

-- | Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
userContextDataType_encodedData :: Lens.Lens' UserContextDataType (Core.Maybe Core.Text)
userContextDataType_encodedData = Lens.lens (\UserContextDataType' {encodedData} -> encodedData) (\s@UserContextDataType' {} a -> s {encodedData = a} :: UserContextDataType)

instance Core.Hashable UserContextDataType

instance Core.NFData UserContextDataType

instance Core.ToJSON UserContextDataType where
  toJSON UserContextDataType' {..} =
    Core.object
      ( Core.catMaybes
          [("EncodedData" Core..=) Core.<$> encodedData]
      )
