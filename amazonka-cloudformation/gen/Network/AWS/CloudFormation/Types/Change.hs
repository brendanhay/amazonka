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
-- Module      : Network.AWS.CloudFormation.Types.Change
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Change where

import Network.AWS.CloudFormation.Types.ChangeType
import Network.AWS.CloudFormation.Types.ResourceChange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The @Change@ structure describes the changes AWS CloudFormation will
-- perform if you execute the change set.
--
-- /See:/ 'newChange' smart constructor.
data Change = Change'
  { -- | A @ResourceChange@ structure that describes the resource and action that
    -- AWS CloudFormation will perform.
    resourceChange :: Prelude.Maybe ResourceChange,
    -- | The type of entity that AWS CloudFormation changes. Currently, the only
    -- entity type is @Resource@.
    type' :: Prelude.Maybe ChangeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Change' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceChange', 'change_resourceChange' - A @ResourceChange@ structure that describes the resource and action that
-- AWS CloudFormation will perform.
--
-- 'type'', 'change_type' - The type of entity that AWS CloudFormation changes. Currently, the only
-- entity type is @Resource@.
newChange ::
  Change
newChange =
  Change'
    { resourceChange = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A @ResourceChange@ structure that describes the resource and action that
-- AWS CloudFormation will perform.
change_resourceChange :: Lens.Lens' Change (Prelude.Maybe ResourceChange)
change_resourceChange = Lens.lens (\Change' {resourceChange} -> resourceChange) (\s@Change' {} a -> s {resourceChange = a} :: Change)

-- | The type of entity that AWS CloudFormation changes. Currently, the only
-- entity type is @Resource@.
change_type :: Lens.Lens' Change (Prelude.Maybe ChangeType)
change_type = Lens.lens (\Change' {type'} -> type') (\s@Change' {} a -> s {type' = a} :: Change)

instance Prelude.FromXML Change where
  parseXML x =
    Change'
      Prelude.<$> (x Prelude..@? "ResourceChange")
      Prelude.<*> (x Prelude..@? "Type")

instance Prelude.Hashable Change

instance Prelude.NFData Change
