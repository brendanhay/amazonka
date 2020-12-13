{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointUser
  ( EndpointUser (..),

    -- * Smart constructor
    mkEndpointUser,

    -- * Lenses
    euUserAttributes,
    euUserId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies data for one or more attributes that describe the user who's associated with an endpoint.
--
-- /See:/ 'mkEndpointUser' smart constructor.
data EndpointUser = EndpointUser'
  { -- | One or more custom attributes that describe the user by associating a name with an array of values. For example, the value of an attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive.
    --
    -- An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
    userAttributes :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    -- | The unique identifier for the user.
    userId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointUser' with the minimum fields required to make a request.
--
-- * 'userAttributes' - One or more custom attributes that describe the user by associating a name with an array of values. For example, the value of an attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive.
--
-- An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
-- * 'userId' - The unique identifier for the user.
mkEndpointUser ::
  EndpointUser
mkEndpointUser =
  EndpointUser'
    { userAttributes = Lude.Nothing,
      userId = Lude.Nothing
    }

-- | One or more custom attributes that describe the user by associating a name with an array of values. For example, the value of an attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive.
--
-- An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
euUserAttributes :: Lens.Lens' EndpointUser (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
euUserAttributes = Lens.lens (userAttributes :: EndpointUser -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {userAttributes = a} :: EndpointUser)
{-# DEPRECATED euUserAttributes "Use generic-lens or generic-optics with 'userAttributes' instead." #-}

-- | The unique identifier for the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
euUserId :: Lens.Lens' EndpointUser (Lude.Maybe Lude.Text)
euUserId = Lens.lens (userId :: EndpointUser -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: EndpointUser)
{-# DEPRECATED euUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.FromJSON EndpointUser where
  parseJSON =
    Lude.withObject
      "EndpointUser"
      ( \x ->
          EndpointUser'
            Lude.<$> (x Lude..:? "UserAttributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "UserId")
      )

instance Lude.ToJSON EndpointUser where
  toJSON EndpointUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UserAttributes" Lude..=) Lude.<$> userAttributes,
            ("UserId" Lude..=) Lude.<$> userId
          ]
      )
