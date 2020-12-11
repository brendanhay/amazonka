-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSecurityGroup
  ( InputSecurityGroup (..),

    -- * Smart constructor
    mkInputSecurityGroup,

    -- * Lenses
    isgState,
    isgARN,
    isgInputs,
    isgId,
    isgWhitelistRules,
    isgTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputSecurityGroupState
import Network.AWS.MediaLive.Types.InputWhitelistRule
import qualified Network.AWS.Prelude as Lude

-- | An Input Security Group
--
-- /See:/ 'mkInputSecurityGroup' smart constructor.
data InputSecurityGroup = InputSecurityGroup'
  { state ::
      Lude.Maybe InputSecurityGroupState,
    arn :: Lude.Maybe Lude.Text,
    inputs :: Lude.Maybe [Lude.Text],
    id :: Lude.Maybe Lude.Text,
    whitelistRules :: Lude.Maybe [InputWhitelistRule],
    tags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputSecurityGroup' with the minimum fields required to make a request.
--
-- * 'arn' - Unique ARN of Input Security Group
-- * 'id' - The Id of the Input Security Group
-- * 'inputs' - The list of inputs currently using this Input Security Group.
-- * 'state' - The current state of the Input Security Group.
-- * 'tags' - A collection of key-value pairs.
-- * 'whitelistRules' - Whitelist rules and their sync status
mkInputSecurityGroup ::
  InputSecurityGroup
mkInputSecurityGroup =
  InputSecurityGroup'
    { state = Lude.Nothing,
      arn = Lude.Nothing,
      inputs = Lude.Nothing,
      id = Lude.Nothing,
      whitelistRules = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The current state of the Input Security Group.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isgState :: Lens.Lens' InputSecurityGroup (Lude.Maybe InputSecurityGroupState)
isgState = Lens.lens (state :: InputSecurityGroup -> Lude.Maybe InputSecurityGroupState) (\s a -> s {state = a} :: InputSecurityGroup)
{-# DEPRECATED isgState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Unique ARN of Input Security Group
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isgARN :: Lens.Lens' InputSecurityGroup (Lude.Maybe Lude.Text)
isgARN = Lens.lens (arn :: InputSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: InputSecurityGroup)
{-# DEPRECATED isgARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The list of inputs currently using this Input Security Group.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isgInputs :: Lens.Lens' InputSecurityGroup (Lude.Maybe [Lude.Text])
isgInputs = Lens.lens (inputs :: InputSecurityGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {inputs = a} :: InputSecurityGroup)
{-# DEPRECATED isgInputs "Use generic-lens or generic-optics with 'inputs' instead." #-}

-- | The Id of the Input Security Group
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isgId :: Lens.Lens' InputSecurityGroup (Lude.Maybe Lude.Text)
isgId = Lens.lens (id :: InputSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: InputSecurityGroup)
{-# DEPRECATED isgId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Whitelist rules and their sync status
--
-- /Note:/ Consider using 'whitelistRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isgWhitelistRules :: Lens.Lens' InputSecurityGroup (Lude.Maybe [InputWhitelistRule])
isgWhitelistRules = Lens.lens (whitelistRules :: InputSecurityGroup -> Lude.Maybe [InputWhitelistRule]) (\s a -> s {whitelistRules = a} :: InputSecurityGroup)
{-# DEPRECATED isgWhitelistRules "Use generic-lens or generic-optics with 'whitelistRules' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isgTags :: Lens.Lens' InputSecurityGroup (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
isgTags = Lens.lens (tags :: InputSecurityGroup -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: InputSecurityGroup)
{-# DEPRECATED isgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON InputSecurityGroup where
  parseJSON =
    Lude.withObject
      "InputSecurityGroup"
      ( \x ->
          InputSecurityGroup'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "inputs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "whitelistRules" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
