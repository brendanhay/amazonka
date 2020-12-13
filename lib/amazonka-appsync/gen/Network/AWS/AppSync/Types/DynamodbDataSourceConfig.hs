{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.DynamodbDataSourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.DynamodbDataSourceConfig
  ( DynamodbDataSourceConfig (..),

    -- * Smart constructor
    mkDynamodbDataSourceConfig,

    -- * Lenses
    ddscVersioned,
    ddscUseCallerCredentials,
    ddscDeltaSyncConfig,
    ddscAwsRegion,
    ddscTableName,
  )
where

import Network.AWS.AppSync.Types.DeltaSyncConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Amazon DynamoDB data source configuration.
--
-- /See:/ 'mkDynamodbDataSourceConfig' smart constructor.
data DynamodbDataSourceConfig = DynamodbDataSourceConfig'
  { -- | Set to TRUE to use Conflict Detection and Resolution with this data source.
    versioned :: Lude.Maybe Lude.Bool,
    -- | Set to TRUE to use Amazon Cognito credentials with this data source.
    useCallerCredentials :: Lude.Maybe Lude.Bool,
    -- | The @DeltaSyncConfig@ for a versioned datasource.
    deltaSyncConfig :: Lude.Maybe DeltaSyncConfig,
    -- | The AWS Region.
    awsRegion :: Lude.Text,
    -- | The table name.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DynamodbDataSourceConfig' with the minimum fields required to make a request.
--
-- * 'versioned' - Set to TRUE to use Conflict Detection and Resolution with this data source.
-- * 'useCallerCredentials' - Set to TRUE to use Amazon Cognito credentials with this data source.
-- * 'deltaSyncConfig' - The @DeltaSyncConfig@ for a versioned datasource.
-- * 'awsRegion' - The AWS Region.
-- * 'tableName' - The table name.
mkDynamodbDataSourceConfig ::
  -- | 'awsRegion'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  DynamodbDataSourceConfig
mkDynamodbDataSourceConfig pAwsRegion_ pTableName_ =
  DynamodbDataSourceConfig'
    { versioned = Lude.Nothing,
      useCallerCredentials = Lude.Nothing,
      deltaSyncConfig = Lude.Nothing,
      awsRegion = pAwsRegion_,
      tableName = pTableName_
    }

-- | Set to TRUE to use Conflict Detection and Resolution with this data source.
--
-- /Note:/ Consider using 'versioned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscVersioned :: Lens.Lens' DynamodbDataSourceConfig (Lude.Maybe Lude.Bool)
ddscVersioned = Lens.lens (versioned :: DynamodbDataSourceConfig -> Lude.Maybe Lude.Bool) (\s a -> s {versioned = a} :: DynamodbDataSourceConfig)
{-# DEPRECATED ddscVersioned "Use generic-lens or generic-optics with 'versioned' instead." #-}

-- | Set to TRUE to use Amazon Cognito credentials with this data source.
--
-- /Note:/ Consider using 'useCallerCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscUseCallerCredentials :: Lens.Lens' DynamodbDataSourceConfig (Lude.Maybe Lude.Bool)
ddscUseCallerCredentials = Lens.lens (useCallerCredentials :: DynamodbDataSourceConfig -> Lude.Maybe Lude.Bool) (\s a -> s {useCallerCredentials = a} :: DynamodbDataSourceConfig)
{-# DEPRECATED ddscUseCallerCredentials "Use generic-lens or generic-optics with 'useCallerCredentials' instead." #-}

-- | The @DeltaSyncConfig@ for a versioned datasource.
--
-- /Note:/ Consider using 'deltaSyncConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscDeltaSyncConfig :: Lens.Lens' DynamodbDataSourceConfig (Lude.Maybe DeltaSyncConfig)
ddscDeltaSyncConfig = Lens.lens (deltaSyncConfig :: DynamodbDataSourceConfig -> Lude.Maybe DeltaSyncConfig) (\s a -> s {deltaSyncConfig = a} :: DynamodbDataSourceConfig)
{-# DEPRECATED ddscDeltaSyncConfig "Use generic-lens or generic-optics with 'deltaSyncConfig' instead." #-}

-- | The AWS Region.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscAwsRegion :: Lens.Lens' DynamodbDataSourceConfig Lude.Text
ddscAwsRegion = Lens.lens (awsRegion :: DynamodbDataSourceConfig -> Lude.Text) (\s a -> s {awsRegion = a} :: DynamodbDataSourceConfig)
{-# DEPRECATED ddscAwsRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

-- | The table name.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscTableName :: Lens.Lens' DynamodbDataSourceConfig Lude.Text
ddscTableName = Lens.lens (tableName :: DynamodbDataSourceConfig -> Lude.Text) (\s a -> s {tableName = a} :: DynamodbDataSourceConfig)
{-# DEPRECATED ddscTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.FromJSON DynamodbDataSourceConfig where
  parseJSON =
    Lude.withObject
      "DynamodbDataSourceConfig"
      ( \x ->
          DynamodbDataSourceConfig'
            Lude.<$> (x Lude..:? "versioned")
            Lude.<*> (x Lude..:? "useCallerCredentials")
            Lude.<*> (x Lude..:? "deltaSyncConfig")
            Lude.<*> (x Lude..: "awsRegion")
            Lude.<*> (x Lude..: "tableName")
      )

instance Lude.ToJSON DynamodbDataSourceConfig where
  toJSON DynamodbDataSourceConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("versioned" Lude..=) Lude.<$> versioned,
            ("useCallerCredentials" Lude..=) Lude.<$> useCallerCredentials,
            ("deltaSyncConfig" Lude..=) Lude.<$> deltaSyncConfig,
            Lude.Just ("awsRegion" Lude..= awsRegion),
            Lude.Just ("tableName" Lude..= tableName)
          ]
      )
