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
-- Module      : Network.AWS.Pinpoint.Types.EndpointUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointUser where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies data for one or more attributes that describe the user who\'s
-- associated with an endpoint.
--
-- /See:/ 'newEndpointUser' smart constructor.
data EndpointUser = EndpointUser'
  { -- | One or more custom attributes that describe the user by associating a
    -- name with an array of values. For example, the value of an attribute
    -- named Interests might be: [\"Science\", \"Music\", \"Travel\"]. You can
    -- use these attributes as filter criteria when you create segments.
    -- Attribute names are case sensitive.
    --
    -- An attribute name can contain up to 50 characters. An attribute value
    -- can contain up to 100 characters. When you define the name of a custom
    -- attribute, avoid using the following characters: number sign (#), colon
    -- (:), question mark (?), backslash (\\), and slash (\/). The Amazon
    -- Pinpoint console can\'t display attribute names that contain these
    -- characters. This restriction doesn\'t apply to attribute values.
    userAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The unique identifier for the user.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userAttributes', 'endpointUser_userAttributes' - One or more custom attributes that describe the user by associating a
-- name with an array of values. For example, the value of an attribute
-- named Interests might be: [\"Science\", \"Music\", \"Travel\"]. You can
-- use these attributes as filter criteria when you create segments.
-- Attribute names are case sensitive.
--
-- An attribute name can contain up to 50 characters. An attribute value
-- can contain up to 100 characters. When you define the name of a custom
-- attribute, avoid using the following characters: number sign (#), colon
-- (:), question mark (?), backslash (\\), and slash (\/). The Amazon
-- Pinpoint console can\'t display attribute names that contain these
-- characters. This restriction doesn\'t apply to attribute values.
--
-- 'userId', 'endpointUser_userId' - The unique identifier for the user.
newEndpointUser ::
  EndpointUser
newEndpointUser =
  EndpointUser'
    { userAttributes = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | One or more custom attributes that describe the user by associating a
-- name with an array of values. For example, the value of an attribute
-- named Interests might be: [\"Science\", \"Music\", \"Travel\"]. You can
-- use these attributes as filter criteria when you create segments.
-- Attribute names are case sensitive.
--
-- An attribute name can contain up to 50 characters. An attribute value
-- can contain up to 100 characters. When you define the name of a custom
-- attribute, avoid using the following characters: number sign (#), colon
-- (:), question mark (?), backslash (\\), and slash (\/). The Amazon
-- Pinpoint console can\'t display attribute names that contain these
-- characters. This restriction doesn\'t apply to attribute values.
endpointUser_userAttributes :: Lens.Lens' EndpointUser (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
endpointUser_userAttributes = Lens.lens (\EndpointUser' {userAttributes} -> userAttributes) (\s@EndpointUser' {} a -> s {userAttributes = a} :: EndpointUser) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for the user.
endpointUser_userId :: Lens.Lens' EndpointUser (Prelude.Maybe Prelude.Text)
endpointUser_userId = Lens.lens (\EndpointUser' {userId} -> userId) (\s@EndpointUser' {} a -> s {userId = a} :: EndpointUser)

instance Core.FromJSON EndpointUser where
  parseJSON =
    Core.withObject
      "EndpointUser"
      ( \x ->
          EndpointUser'
            Prelude.<$> (x Core..:? "UserAttributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "UserId")
      )

instance Prelude.Hashable EndpointUser

instance Prelude.NFData EndpointUser

instance Core.ToJSON EndpointUser where
  toJSON EndpointUser' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UserAttributes" Core..=)
              Prelude.<$> userAttributes,
            ("UserId" Core..=) Prelude.<$> userId
          ]
      )
