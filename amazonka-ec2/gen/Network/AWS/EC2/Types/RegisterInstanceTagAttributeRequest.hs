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
-- Module      : Network.AWS.EC2.Types.RegisterInstanceTagAttributeRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RegisterInstanceTagAttributeRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Information about the tag keys to register for the current Region. You
-- can either specify individual tag keys or register all tag keys in the
-- current Region. You must specify either @IncludeAllTagsOfInstance@ or
-- @InstanceTagKeys@ in the request
--
-- /See:/ 'newRegisterInstanceTagAttributeRequest' smart constructor.
data RegisterInstanceTagAttributeRequest = RegisterInstanceTagAttributeRequest'
  { -- | The tag keys to register.
    instanceTagKeys :: Core.Maybe [Core.Text],
    -- | Indicates whether to register all tag keys in the current Region.
    -- Specify @true@ to register all tag keys.
    includeAllTagsOfInstance :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterInstanceTagAttributeRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceTagKeys', 'registerInstanceTagAttributeRequest_instanceTagKeys' - The tag keys to register.
--
-- 'includeAllTagsOfInstance', 'registerInstanceTagAttributeRequest_includeAllTagsOfInstance' - Indicates whether to register all tag keys in the current Region.
-- Specify @true@ to register all tag keys.
newRegisterInstanceTagAttributeRequest ::
  RegisterInstanceTagAttributeRequest
newRegisterInstanceTagAttributeRequest =
  RegisterInstanceTagAttributeRequest'
    { instanceTagKeys =
        Core.Nothing,
      includeAllTagsOfInstance =
        Core.Nothing
    }

-- | The tag keys to register.
registerInstanceTagAttributeRequest_instanceTagKeys :: Lens.Lens' RegisterInstanceTagAttributeRequest (Core.Maybe [Core.Text])
registerInstanceTagAttributeRequest_instanceTagKeys = Lens.lens (\RegisterInstanceTagAttributeRequest' {instanceTagKeys} -> instanceTagKeys) (\s@RegisterInstanceTagAttributeRequest' {} a -> s {instanceTagKeys = a} :: RegisterInstanceTagAttributeRequest) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether to register all tag keys in the current Region.
-- Specify @true@ to register all tag keys.
registerInstanceTagAttributeRequest_includeAllTagsOfInstance :: Lens.Lens' RegisterInstanceTagAttributeRequest (Core.Maybe Core.Bool)
registerInstanceTagAttributeRequest_includeAllTagsOfInstance = Lens.lens (\RegisterInstanceTagAttributeRequest' {includeAllTagsOfInstance} -> includeAllTagsOfInstance) (\s@RegisterInstanceTagAttributeRequest' {} a -> s {includeAllTagsOfInstance = a} :: RegisterInstanceTagAttributeRequest)

instance
  Core.Hashable
    RegisterInstanceTagAttributeRequest

instance
  Core.NFData
    RegisterInstanceTagAttributeRequest

instance
  Core.ToQuery
    RegisterInstanceTagAttributeRequest
  where
  toQuery RegisterInstanceTagAttributeRequest' {..} =
    Core.mconcat
      [ Core.toQuery
          ( Core.toQueryList "InstanceTagKey"
              Core.<$> instanceTagKeys
          ),
        "IncludeAllTagsOfInstance"
          Core.=: includeAllTagsOfInstance
      ]
