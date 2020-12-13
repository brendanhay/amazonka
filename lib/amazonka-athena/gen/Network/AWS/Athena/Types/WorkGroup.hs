{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.WorkGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.WorkGroup
  ( WorkGroup (..),

    -- * Smart constructor
    mkWorkGroup,

    -- * Lenses
    wgCreationTime,
    wgState,
    wgName,
    wgConfiguration,
    wgDescription,
  )
where

import Network.AWS.Athena.Types.WorkGroupConfiguration
import Network.AWS.Athena.Types.WorkGroupState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A workgroup, which contains a name, description, creation time, state, and other configuration, listed under 'WorkGroup$Configuration' . Each workgroup enables you to isolate queries for you or your group of users from other queries in the same account, to configure the query results location and the encryption configuration (known as workgroup settings), to enable sending query metrics to Amazon CloudWatch, and to establish per-query data usage control limits for all queries in a workgroup. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- /See:/ 'mkWorkGroup' smart constructor.
data WorkGroup = WorkGroup'
  { -- | The date and time the workgroup was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The state of the workgroup: ENABLED or DISABLED.
    state :: Lude.Maybe WorkGroupState,
    -- | The workgroup name.
    name :: Lude.Text,
    -- | The configuration of the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption configuration, if any, used for query results; whether the Amazon CloudWatch Metrics are enabled for the workgroup; whether workgroup settings override client-side settings; and the data usage limits for the amount of data scanned per query or per workgroup. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
    configuration :: Lude.Maybe WorkGroupConfiguration,
    -- | The workgroup description.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkGroup' with the minimum fields required to make a request.
--
-- * 'creationTime' - The date and time the workgroup was created.
-- * 'state' - The state of the workgroup: ENABLED or DISABLED.
-- * 'name' - The workgroup name.
-- * 'configuration' - The configuration of the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption configuration, if any, used for query results; whether the Amazon CloudWatch Metrics are enabled for the workgroup; whether workgroup settings override client-side settings; and the data usage limits for the amount of data scanned per query or per workgroup. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
-- * 'description' - The workgroup description.
mkWorkGroup ::
  -- | 'name'
  Lude.Text ->
  WorkGroup
mkWorkGroup pName_ =
  WorkGroup'
    { creationTime = Lude.Nothing,
      state = Lude.Nothing,
      name = pName_,
      configuration = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The date and time the workgroup was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgCreationTime :: Lens.Lens' WorkGroup (Lude.Maybe Lude.Timestamp)
wgCreationTime = Lens.lens (creationTime :: WorkGroup -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: WorkGroup)
{-# DEPRECATED wgCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The state of the workgroup: ENABLED or DISABLED.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgState :: Lens.Lens' WorkGroup (Lude.Maybe WorkGroupState)
wgState = Lens.lens (state :: WorkGroup -> Lude.Maybe WorkGroupState) (\s a -> s {state = a} :: WorkGroup)
{-# DEPRECATED wgState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The workgroup name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgName :: Lens.Lens' WorkGroup Lude.Text
wgName = Lens.lens (name :: WorkGroup -> Lude.Text) (\s a -> s {name = a} :: WorkGroup)
{-# DEPRECATED wgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The configuration of the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption configuration, if any, used for query results; whether the Amazon CloudWatch Metrics are enabled for the workgroup; whether workgroup settings override client-side settings; and the data usage limits for the amount of data scanned per query or per workgroup. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgConfiguration :: Lens.Lens' WorkGroup (Lude.Maybe WorkGroupConfiguration)
wgConfiguration = Lens.lens (configuration :: WorkGroup -> Lude.Maybe WorkGroupConfiguration) (\s a -> s {configuration = a} :: WorkGroup)
{-# DEPRECATED wgConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The workgroup description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgDescription :: Lens.Lens' WorkGroup (Lude.Maybe Lude.Text)
wgDescription = Lens.lens (description :: WorkGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: WorkGroup)
{-# DEPRECATED wgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON WorkGroup where
  parseJSON =
    Lude.withObject
      "WorkGroup"
      ( \x ->
          WorkGroup'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..:? "Configuration")
            Lude.<*> (x Lude..:? "Description")
      )
