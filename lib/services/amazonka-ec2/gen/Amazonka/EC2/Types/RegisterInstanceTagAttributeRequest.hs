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
-- Module      : Amazonka.EC2.Types.RegisterInstanceTagAttributeRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RegisterInstanceTagAttributeRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about the tag keys to register for the current Region. You
-- can either specify individual tag keys or register all tag keys in the
-- current Region. You must specify either @IncludeAllTagsOfInstance@ or
-- @InstanceTagKeys@ in the request
--
-- /See:/ 'newRegisterInstanceTagAttributeRequest' smart constructor.
data RegisterInstanceTagAttributeRequest = RegisterInstanceTagAttributeRequest'
  { -- | Indicates whether to register all tag keys in the current Region.
    -- Specify @true@ to register all tag keys.
    includeAllTagsOfInstance :: Prelude.Maybe Prelude.Bool,
    -- | The tag keys to register.
    instanceTagKeys :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterInstanceTagAttributeRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeAllTagsOfInstance', 'registerInstanceTagAttributeRequest_includeAllTagsOfInstance' - Indicates whether to register all tag keys in the current Region.
-- Specify @true@ to register all tag keys.
--
-- 'instanceTagKeys', 'registerInstanceTagAttributeRequest_instanceTagKeys' - The tag keys to register.
newRegisterInstanceTagAttributeRequest ::
  RegisterInstanceTagAttributeRequest
newRegisterInstanceTagAttributeRequest =
  RegisterInstanceTagAttributeRequest'
    { includeAllTagsOfInstance =
        Prelude.Nothing,
      instanceTagKeys = Prelude.Nothing
    }

-- | Indicates whether to register all tag keys in the current Region.
-- Specify @true@ to register all tag keys.
registerInstanceTagAttributeRequest_includeAllTagsOfInstance :: Lens.Lens' RegisterInstanceTagAttributeRequest (Prelude.Maybe Prelude.Bool)
registerInstanceTagAttributeRequest_includeAllTagsOfInstance = Lens.lens (\RegisterInstanceTagAttributeRequest' {includeAllTagsOfInstance} -> includeAllTagsOfInstance) (\s@RegisterInstanceTagAttributeRequest' {} a -> s {includeAllTagsOfInstance = a} :: RegisterInstanceTagAttributeRequest)

-- | The tag keys to register.
registerInstanceTagAttributeRequest_instanceTagKeys :: Lens.Lens' RegisterInstanceTagAttributeRequest (Prelude.Maybe [Prelude.Text])
registerInstanceTagAttributeRequest_instanceTagKeys = Lens.lens (\RegisterInstanceTagAttributeRequest' {instanceTagKeys} -> instanceTagKeys) (\s@RegisterInstanceTagAttributeRequest' {} a -> s {instanceTagKeys = a} :: RegisterInstanceTagAttributeRequest) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    RegisterInstanceTagAttributeRequest
  where
  hashWithSalt
    _salt
    RegisterInstanceTagAttributeRequest' {..} =
      _salt
        `Prelude.hashWithSalt` includeAllTagsOfInstance
        `Prelude.hashWithSalt` instanceTagKeys

instance
  Prelude.NFData
    RegisterInstanceTagAttributeRequest
  where
  rnf RegisterInstanceTagAttributeRequest' {..} =
    Prelude.rnf includeAllTagsOfInstance
      `Prelude.seq` Prelude.rnf instanceTagKeys

instance
  Core.ToQuery
    RegisterInstanceTagAttributeRequest
  where
  toQuery RegisterInstanceTagAttributeRequest' {..} =
    Prelude.mconcat
      [ "IncludeAllTagsOfInstance"
          Core.=: includeAllTagsOfInstance,
        Core.toQuery
          ( Core.toQueryList "InstanceTagKey"
              Prelude.<$> instanceTagKeys
          )
      ]
