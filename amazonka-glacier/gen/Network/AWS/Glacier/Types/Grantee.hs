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
-- Module      : Network.AWS.Glacier.Types.Grantee
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.Grantee where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types.Type
import qualified Network.AWS.Lens as Lens

-- | Contains information about the grantee.
--
-- /See:/ 'newGrantee' smart constructor.
data Grantee = Grantee'
  { -- | URI of the grantee group.
    uri :: Core.Maybe Core.Text,
    -- | The canonical user ID of the grantee.
    id :: Core.Maybe Core.Text,
    -- | Screen name of the grantee.
    displayName :: Core.Maybe Core.Text,
    -- | Email address of the grantee.
    emailAddress :: Core.Maybe Core.Text,
    -- | Type of grantee
    type' :: Type
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Grantee' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uri', 'grantee_uri' - URI of the grantee group.
--
-- 'id', 'grantee_id' - The canonical user ID of the grantee.
--
-- 'displayName', 'grantee_displayName' - Screen name of the grantee.
--
-- 'emailAddress', 'grantee_emailAddress' - Email address of the grantee.
--
-- 'type'', 'grantee_type' - Type of grantee
newGrantee ::
  -- | 'type''
  Type ->
  Grantee
newGrantee pType_ =
  Grantee'
    { uri = Core.Nothing,
      id = Core.Nothing,
      displayName = Core.Nothing,
      emailAddress = Core.Nothing,
      type' = pType_
    }

-- | URI of the grantee group.
grantee_uri :: Lens.Lens' Grantee (Core.Maybe Core.Text)
grantee_uri = Lens.lens (\Grantee' {uri} -> uri) (\s@Grantee' {} a -> s {uri = a} :: Grantee)

-- | The canonical user ID of the grantee.
grantee_id :: Lens.Lens' Grantee (Core.Maybe Core.Text)
grantee_id = Lens.lens (\Grantee' {id} -> id) (\s@Grantee' {} a -> s {id = a} :: Grantee)

-- | Screen name of the grantee.
grantee_displayName :: Lens.Lens' Grantee (Core.Maybe Core.Text)
grantee_displayName = Lens.lens (\Grantee' {displayName} -> displayName) (\s@Grantee' {} a -> s {displayName = a} :: Grantee)

-- | Email address of the grantee.
grantee_emailAddress :: Lens.Lens' Grantee (Core.Maybe Core.Text)
grantee_emailAddress = Lens.lens (\Grantee' {emailAddress} -> emailAddress) (\s@Grantee' {} a -> s {emailAddress = a} :: Grantee)

-- | Type of grantee
grantee_type :: Lens.Lens' Grantee Type
grantee_type = Lens.lens (\Grantee' {type'} -> type') (\s@Grantee' {} a -> s {type' = a} :: Grantee)

instance Core.FromJSON Grantee where
  parseJSON =
    Core.withObject
      "Grantee"
      ( \x ->
          Grantee'
            Core.<$> (x Core..:? "URI")
            Core.<*> (x Core..:? "ID")
            Core.<*> (x Core..:? "DisplayName")
            Core.<*> (x Core..:? "EmailAddress")
            Core.<*> (x Core..: "Type")
      )

instance Core.Hashable Grantee

instance Core.NFData Grantee

instance Core.ToJSON Grantee where
  toJSON Grantee' {..} =
    Core.object
      ( Core.catMaybes
          [ ("URI" Core..=) Core.<$> uri,
            ("ID" Core..=) Core.<$> id,
            ("DisplayName" Core..=) Core.<$> displayName,
            ("EmailAddress" Core..=) Core.<$> emailAddress,
            Core.Just ("Type" Core..= type')
          ]
      )
