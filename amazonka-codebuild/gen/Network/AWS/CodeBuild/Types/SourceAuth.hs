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
-- Module      : Network.AWS.CodeBuild.Types.SourceAuth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.SourceAuth where

import Network.AWS.CodeBuild.Types.SourceAuthType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the authorization settings for AWS CodeBuild to access
-- the source code to be built.
--
-- This information is for the AWS CodeBuild console\'s use only. Your code
-- should not get or set this information directly.
--
-- /See:/ 'newSourceAuth' smart constructor.
data SourceAuth = SourceAuth'
  { -- | The resource value that applies to the specified authorization type.
    resource :: Prelude.Maybe Prelude.Text,
    -- | This data type is deprecated and is no longer accurate or used.
    --
    -- The authorization type to use. The only valid value is @OAUTH@, which
    -- represents the OAuth authorization type.
    type' :: SourceAuthType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SourceAuth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resource', 'sourceAuth_resource' - The resource value that applies to the specified authorization type.
--
-- 'type'', 'sourceAuth_type' - This data type is deprecated and is no longer accurate or used.
--
-- The authorization type to use. The only valid value is @OAUTH@, which
-- represents the OAuth authorization type.
newSourceAuth ::
  -- | 'type''
  SourceAuthType ->
  SourceAuth
newSourceAuth pType_ =
  SourceAuth'
    { resource = Prelude.Nothing,
      type' = pType_
    }

-- | The resource value that applies to the specified authorization type.
sourceAuth_resource :: Lens.Lens' SourceAuth (Prelude.Maybe Prelude.Text)
sourceAuth_resource = Lens.lens (\SourceAuth' {resource} -> resource) (\s@SourceAuth' {} a -> s {resource = a} :: SourceAuth)

-- | This data type is deprecated and is no longer accurate or used.
--
-- The authorization type to use. The only valid value is @OAUTH@, which
-- represents the OAuth authorization type.
sourceAuth_type :: Lens.Lens' SourceAuth SourceAuthType
sourceAuth_type = Lens.lens (\SourceAuth' {type'} -> type') (\s@SourceAuth' {} a -> s {type' = a} :: SourceAuth)

instance Prelude.FromJSON SourceAuth where
  parseJSON =
    Prelude.withObject
      "SourceAuth"
      ( \x ->
          SourceAuth'
            Prelude.<$> (x Prelude..:? "resource")
            Prelude.<*> (x Prelude..: "type")
      )

instance Prelude.Hashable SourceAuth

instance Prelude.NFData SourceAuth

instance Prelude.ToJSON SourceAuth where
  toJSON SourceAuth' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("resource" Prelude..=) Prelude.<$> resource,
            Prelude.Just ("type" Prelude..= type')
          ]
      )
