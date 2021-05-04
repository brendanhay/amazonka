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
-- Module      : Network.AWS.EC2.Types.DeregisterInstanceTagAttributeRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeregisterInstanceTagAttributeRequest where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the tag keys to deregister for the current Region. You
-- can either specify individual tag keys or deregister all tag keys in the
-- current Region. You must specify either @IncludeAllTagsOfInstance@ or
-- @InstanceTagKeys@ in the request
--
-- /See:/ 'newDeregisterInstanceTagAttributeRequest' smart constructor.
data DeregisterInstanceTagAttributeRequest = DeregisterInstanceTagAttributeRequest'
  { -- | Information about the tag keys to deregister.
    instanceTagKeys :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether to deregister all tag keys in the current Region.
    -- Specify @false@ to deregister all tag keys.
    includeAllTagsOfInstance :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterInstanceTagAttributeRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceTagKeys', 'deregisterInstanceTagAttributeRequest_instanceTagKeys' - Information about the tag keys to deregister.
--
-- 'includeAllTagsOfInstance', 'deregisterInstanceTagAttributeRequest_includeAllTagsOfInstance' - Indicates whether to deregister all tag keys in the current Region.
-- Specify @false@ to deregister all tag keys.
newDeregisterInstanceTagAttributeRequest ::
  DeregisterInstanceTagAttributeRequest
newDeregisterInstanceTagAttributeRequest =
  DeregisterInstanceTagAttributeRequest'
    { instanceTagKeys =
        Prelude.Nothing,
      includeAllTagsOfInstance =
        Prelude.Nothing
    }

-- | Information about the tag keys to deregister.
deregisterInstanceTagAttributeRequest_instanceTagKeys :: Lens.Lens' DeregisterInstanceTagAttributeRequest (Prelude.Maybe [Prelude.Text])
deregisterInstanceTagAttributeRequest_instanceTagKeys = Lens.lens (\DeregisterInstanceTagAttributeRequest' {instanceTagKeys} -> instanceTagKeys) (\s@DeregisterInstanceTagAttributeRequest' {} a -> s {instanceTagKeys = a} :: DeregisterInstanceTagAttributeRequest) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether to deregister all tag keys in the current Region.
-- Specify @false@ to deregister all tag keys.
deregisterInstanceTagAttributeRequest_includeAllTagsOfInstance :: Lens.Lens' DeregisterInstanceTagAttributeRequest (Prelude.Maybe Prelude.Bool)
deregisterInstanceTagAttributeRequest_includeAllTagsOfInstance = Lens.lens (\DeregisterInstanceTagAttributeRequest' {includeAllTagsOfInstance} -> includeAllTagsOfInstance) (\s@DeregisterInstanceTagAttributeRequest' {} a -> s {includeAllTagsOfInstance = a} :: DeregisterInstanceTagAttributeRequest)

instance
  Prelude.Hashable
    DeregisterInstanceTagAttributeRequest

instance
  Prelude.NFData
    DeregisterInstanceTagAttributeRequest

instance
  Prelude.ToQuery
    DeregisterInstanceTagAttributeRequest
  where
  toQuery DeregisterInstanceTagAttributeRequest' {..} =
    Prelude.mconcat
      [ Prelude.toQuery
          ( Prelude.toQueryList "InstanceTagKey"
              Prelude.<$> instanceTagKeys
          ),
        "IncludeAllTagsOfInstance"
          Prelude.=: includeAllTagsOfInstance
      ]
