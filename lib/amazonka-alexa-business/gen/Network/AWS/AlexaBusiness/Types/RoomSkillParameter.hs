-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.RoomSkillParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.RoomSkillParameter
  ( RoomSkillParameter (..),

    -- * Smart constructor
    mkRoomSkillParameter,

    -- * Lenses
    rspParameterKey,
    rspParameterValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A skill parameter associated with a room.
--
-- /See:/ 'mkRoomSkillParameter' smart constructor.
data RoomSkillParameter = RoomSkillParameter'
  { parameterKey ::
      Lude.Text,
    parameterValue :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RoomSkillParameter' with the minimum fields required to make a request.
--
-- * 'parameterKey' - The parameter key of a room skill parameter. ParameterKey is an enumerated type that only takes “DEFAULT” or “SCOPE” as valid values.
-- * 'parameterValue' - The parameter value of a room skill parameter.
mkRoomSkillParameter ::
  -- | 'parameterKey'
  Lude.Text ->
  -- | 'parameterValue'
  Lude.Text ->
  RoomSkillParameter
mkRoomSkillParameter pParameterKey_ pParameterValue_ =
  RoomSkillParameter'
    { parameterKey = pParameterKey_,
      parameterValue = pParameterValue_
    }

-- | The parameter key of a room skill parameter. ParameterKey is an enumerated type that only takes “DEFAULT” or “SCOPE” as valid values.
--
-- /Note:/ Consider using 'parameterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rspParameterKey :: Lens.Lens' RoomSkillParameter Lude.Text
rspParameterKey = Lens.lens (parameterKey :: RoomSkillParameter -> Lude.Text) (\s a -> s {parameterKey = a} :: RoomSkillParameter)
{-# DEPRECATED rspParameterKey "Use generic-lens or generic-optics with 'parameterKey' instead." #-}

-- | The parameter value of a room skill parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rspParameterValue :: Lens.Lens' RoomSkillParameter Lude.Text
rspParameterValue = Lens.lens (parameterValue :: RoomSkillParameter -> Lude.Text) (\s a -> s {parameterValue = a} :: RoomSkillParameter)
{-# DEPRECATED rspParameterValue "Use generic-lens or generic-optics with 'parameterValue' instead." #-}

instance Lude.FromJSON RoomSkillParameter where
  parseJSON =
    Lude.withObject
      "RoomSkillParameter"
      ( \x ->
          RoomSkillParameter'
            Lude.<$> (x Lude..: "ParameterKey") Lude.<*> (x Lude..: "ParameterValue")
      )

instance Lude.ToJSON RoomSkillParameter where
  toJSON RoomSkillParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ParameterKey" Lude..= parameterKey),
            Lude.Just ("ParameterValue" Lude..= parameterValue)
          ]
      )
