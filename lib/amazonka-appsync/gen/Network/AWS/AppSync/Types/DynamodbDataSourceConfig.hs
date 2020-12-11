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
    ddscTableName,
    ddscAwsRegion,
  )
where

import Network.AWS.AppSync.Types.DeltaSyncConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Amazon DynamoDB data source configuration.
--
-- /See:/ 'mkDynamodbDataSourceConfig' smart constructor.
data DynamodbDataSourceConfig = DynamodbDataSourceConfig'
  { versioned ::
      Lude.Maybe Lude.Bool,
    useCallerCredentials ::
      Lude.Maybe Lude.Bool,
    deltaSyncConfig ::
      Lude.Maybe DeltaSyncConfig,
    tableName :: Lude.Text,
    awsRegion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DynamodbDataSourceConfig' with the minimum fields required to make a request.
--
-- * 'awsRegion' - The AWS Region.
-- * 'deltaSyncConfig' - The @DeltaSyncConfig@ for a versioned datasource.
-- * 'tableName' - The table name.
-- * 'useCallerCredentials' - Set to TRUE to use Amazon Cognito credentials with this data source.
-- * 'versioned' - Set to TRUE to use Conflict Detection and Resolution with this data source.
mkDynamodbDataSourceConfig ::
  -- | 'tableName'
  Lude.Text ->
  -- | 'awsRegion'
  Lude.Text ->
  DynamodbDataSourceConfig
mkDynamodbDataSourceConfig pTableName_ pAwsRegion_ =
  DynamodbDataSourceConfig'
    { versioned = Lude.Nothing,
      useCallerCredentials = Lude.Nothing,
      deltaSyncConfig = Lude.Nothing,
      tableName = pTableName_,
      awsRegion = pAwsRegion_
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

-- | The table name.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscTableName :: Lens.Lens' DynamodbDataSourceConfig Lude.Text
ddscTableName = Lens.lens (tableName :: DynamodbDataSourceConfig -> Lude.Text) (\s a -> s {tableName = a} :: DynamodbDataSourceConfig)
{-# DEPRECATED ddscTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The AWS Region.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscAwsRegion :: Lens.Lens' DynamodbDataSourceConfig Lude.Text
ddscAwsRegion = Lens.lens (awsRegion :: DynamodbDataSourceConfig -> Lude.Text) (\s a -> s {awsRegion = a} :: DynamodbDataSourceConfig)
{-# DEPRECATED ddscAwsRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

instance Lude.FromJSON DynamodbDataSourceConfig where
  parseJSON =
    Lude.withObject
      "DynamodbDataSourceConfig"
      ( \x ->
          DynamodbDataSourceConfig'
            Lude.<$> (x Lude..:? "versioned")
            Lude.<*> (x Lude..:? "useCallerCredentials")
            Lude.<*> (x Lude..:? "deltaSyncConfig")
            Lude.<*> (x Lude..: "tableName")
            Lude.<*> (x Lude..: "awsRegion")
      )

instance Lude.ToJSON DynamodbDataSourceConfig where
  toJSON DynamodbDataSourceConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("versioned" Lude..=) Lude.<$> versioned,
            ("useCallerCredentials" Lude..=) Lude.<$> useCallerCredentials,
            ("deltaSyncConfig" Lude..=) Lude.<$> deltaSyncConfig,
            Lude.Just ("tableName" Lude..= tableName),
            Lude.Just ("awsRegion" Lude..= awsRegion)
          ]
      )
