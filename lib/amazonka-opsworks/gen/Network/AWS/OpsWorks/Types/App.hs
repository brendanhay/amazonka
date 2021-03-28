{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.App
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.App
  ( App (..)
  -- * Smart constructor
  , mkApp
  -- * Lenses
  , aAppId
  , aAppSource
  , aAttributes
  , aCreatedAt
  , aDataSources
  , aDescription
  , aDomains
  , aEnableSsl
  , aEnvironment
  , aName
  , aShortname
  , aSslConfiguration
  , aStackId
  , aType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.AppAttributesKeys as Types
import qualified Network.AWS.OpsWorks.Types.AppType as Types
import qualified Network.AWS.OpsWorks.Types.DataSource as Types
import qualified Network.AWS.OpsWorks.Types.EnvironmentVariable as Types
import qualified Network.AWS.OpsWorks.Types.Source as Types
import qualified Network.AWS.OpsWorks.Types.SslConfiguration as Types
import qualified Network.AWS.Prelude as Core

-- | A description of the app.
--
-- /See:/ 'mkApp' smart constructor.
data App = App'
  { appId :: Core.Maybe Core.Text
    -- ^ The app ID.
  , appSource :: Core.Maybe Types.Source
    -- ^ A @Source@ object that describes the app repository.
  , attributes :: Core.Maybe (Core.HashMap Types.AppAttributesKeys Core.Text)
    -- ^ The stack attributes.
  , createdAt :: Core.Maybe Core.Text
    -- ^ When the app was created.
  , dataSources :: Core.Maybe [Types.DataSource]
    -- ^ The app's data sources.
  , description :: Core.Maybe Core.Text
    -- ^ A description of the app.
  , domains :: Core.Maybe [Core.Text]
    -- ^ The app vhost settings with multiple domains separated by commas. For example: @'www.example.com, example.com'@ 
  , enableSsl :: Core.Maybe Core.Bool
    -- ^ Whether to enable SSL for the app.
  , environment :: Core.Maybe [Types.EnvironmentVariable]
    -- ^ An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> . 
  , name :: Core.Maybe Core.Text
    -- ^ The app name.
  , shortname :: Core.Maybe Core.Text
    -- ^ The app's short name.
  , sslConfiguration :: Core.Maybe Types.SslConfiguration
    -- ^ An @SslConfiguration@ object with the SSL configuration.
  , stackId :: Core.Maybe Core.Text
    -- ^ The app stack ID.
  , type' :: Core.Maybe Types.AppType
    -- ^ The app type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'App' value with any optional fields omitted.
mkApp
    :: App
mkApp
  = App'{appId = Core.Nothing, appSource = Core.Nothing,
         attributes = Core.Nothing, createdAt = Core.Nothing,
         dataSources = Core.Nothing, description = Core.Nothing,
         domains = Core.Nothing, enableSsl = Core.Nothing,
         environment = Core.Nothing, name = Core.Nothing,
         shortname = Core.Nothing, sslConfiguration = Core.Nothing,
         stackId = Core.Nothing, type' = Core.Nothing}

-- | The app ID.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAppId :: Lens.Lens' App (Core.Maybe Core.Text)
aAppId = Lens.field @"appId"
{-# INLINEABLE aAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

-- | A @Source@ object that describes the app repository.
--
-- /Note:/ Consider using 'appSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAppSource :: Lens.Lens' App (Core.Maybe Types.Source)
aAppSource = Lens.field @"appSource"
{-# INLINEABLE aAppSource #-}
{-# DEPRECATED appSource "Use generic-lens or generic-optics with 'appSource' instead"  #-}

-- | The stack attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAttributes :: Lens.Lens' App (Core.Maybe (Core.HashMap Types.AppAttributesKeys Core.Text))
aAttributes = Lens.field @"attributes"
{-# INLINEABLE aAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | When the app was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreatedAt :: Lens.Lens' App (Core.Maybe Core.Text)
aCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE aCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The app's data sources.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDataSources :: Lens.Lens' App (Core.Maybe [Types.DataSource])
aDataSources = Lens.field @"dataSources"
{-# INLINEABLE aDataSources #-}
{-# DEPRECATED dataSources "Use generic-lens or generic-optics with 'dataSources' instead"  #-}

-- | A description of the app.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDescription :: Lens.Lens' App (Core.Maybe Core.Text)
aDescription = Lens.field @"description"
{-# INLINEABLE aDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The app vhost settings with multiple domains separated by commas. For example: @'www.example.com, example.com'@ 
--
-- /Note:/ Consider using 'domains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDomains :: Lens.Lens' App (Core.Maybe [Core.Text])
aDomains = Lens.field @"domains"
{-# INLINEABLE aDomains #-}
{-# DEPRECATED domains "Use generic-lens or generic-optics with 'domains' instead"  #-}

-- | Whether to enable SSL for the app.
--
-- /Note:/ Consider using 'enableSsl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEnableSsl :: Lens.Lens' App (Core.Maybe Core.Bool)
aEnableSsl = Lens.field @"enableSsl"
{-# INLINEABLE aEnableSsl #-}
{-# DEPRECATED enableSsl "Use generic-lens or generic-optics with 'enableSsl' instead"  #-}

-- | An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> . 
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEnvironment :: Lens.Lens' App (Core.Maybe [Types.EnvironmentVariable])
aEnvironment = Lens.field @"environment"
{-# INLINEABLE aEnvironment #-}
{-# DEPRECATED environment "Use generic-lens or generic-optics with 'environment' instead"  #-}

-- | The app name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' App (Core.Maybe Core.Text)
aName = Lens.field @"name"
{-# INLINEABLE aName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The app's short name.
--
-- /Note:/ Consider using 'shortname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aShortname :: Lens.Lens' App (Core.Maybe Core.Text)
aShortname = Lens.field @"shortname"
{-# INLINEABLE aShortname #-}
{-# DEPRECATED shortname "Use generic-lens or generic-optics with 'shortname' instead"  #-}

-- | An @SslConfiguration@ object with the SSL configuration.
--
-- /Note:/ Consider using 'sslConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSslConfiguration :: Lens.Lens' App (Core.Maybe Types.SslConfiguration)
aSslConfiguration = Lens.field @"sslConfiguration"
{-# INLINEABLE aSslConfiguration #-}
{-# DEPRECATED sslConfiguration "Use generic-lens or generic-optics with 'sslConfiguration' instead"  #-}

-- | The app stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStackId :: Lens.Lens' App (Core.Maybe Core.Text)
aStackId = Lens.field @"stackId"
{-# INLINEABLE aStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | The app type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aType :: Lens.Lens' App (Core.Maybe Types.AppType)
aType = Lens.field @"type'"
{-# INLINEABLE aType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON App where
        parseJSON
          = Core.withObject "App" Core.$
              \ x ->
                App' Core.<$>
                  (x Core..:? "AppId") Core.<*> x Core..:? "AppSource" Core.<*>
                    x Core..:? "Attributes"
                    Core.<*> x Core..:? "CreatedAt"
                    Core.<*> x Core..:? "DataSources"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "Domains"
                    Core.<*> x Core..:? "EnableSsl"
                    Core.<*> x Core..:? "Environment"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Shortname"
                    Core.<*> x Core..:? "SslConfiguration"
                    Core.<*> x Core..:? "StackId"
                    Core.<*> x Core..:? "Type"
