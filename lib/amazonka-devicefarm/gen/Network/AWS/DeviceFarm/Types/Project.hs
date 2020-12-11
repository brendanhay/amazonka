-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Project
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Project
  ( Project (..),

    -- * Smart constructor
    mkProject,

    -- * Lenses
    pArn,
    pCreated,
    pName,
    pDefaultJobTimeoutMinutes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an operating-system neutral workspace for running and managing tests.
--
-- /See:/ 'mkProject' smart constructor.
data Project = Project'
  { arn :: Lude.Maybe Lude.Text,
    created :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    defaultJobTimeoutMinutes :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Project' with the minimum fields required to make a request.
--
-- * 'arn' - The project's ARN.
-- * 'created' - When the project was created.
-- * 'defaultJobTimeoutMinutes' - The default number of minutes (at the project level) a test run executes before it times out. The default value is 150 minutes.
-- * 'name' - The project's name.
mkProject ::
  Project
mkProject =
  Project'
    { arn = Lude.Nothing,
      created = Lude.Nothing,
      name = Lude.Nothing,
      defaultJobTimeoutMinutes = Lude.Nothing
    }

-- | The project's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pArn :: Lens.Lens' Project (Lude.Maybe Lude.Text)
pArn = Lens.lens (arn :: Project -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Project)
{-# DEPRECATED pArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | When the project was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCreated :: Lens.Lens' Project (Lude.Maybe Lude.Timestamp)
pCreated = Lens.lens (created :: Project -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: Project)
{-# DEPRECATED pCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The project's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pName :: Lens.Lens' Project (Lude.Maybe Lude.Text)
pName = Lens.lens (name :: Project -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Project)
{-# DEPRECATED pName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The default number of minutes (at the project level) a test run executes before it times out. The default value is 150 minutes.
--
-- /Note:/ Consider using 'defaultJobTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDefaultJobTimeoutMinutes :: Lens.Lens' Project (Lude.Maybe Lude.Int)
pDefaultJobTimeoutMinutes = Lens.lens (defaultJobTimeoutMinutes :: Project -> Lude.Maybe Lude.Int) (\s a -> s {defaultJobTimeoutMinutes = a} :: Project)
{-# DEPRECATED pDefaultJobTimeoutMinutes "Use generic-lens or generic-optics with 'defaultJobTimeoutMinutes' instead." #-}

instance Lude.FromJSON Project where
  parseJSON =
    Lude.withObject
      "Project"
      ( \x ->
          Project'
            Lude.<$> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "defaultJobTimeoutMinutes")
      )
