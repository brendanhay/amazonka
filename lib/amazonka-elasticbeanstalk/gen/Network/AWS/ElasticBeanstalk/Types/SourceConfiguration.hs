{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SourceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SourceConfiguration
  ( SourceConfiguration (..),

    -- * Smart constructor
    mkSourceConfiguration,

    -- * Lenses
    scTemplateName,
    scApplicationName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A specification for an environment configuration.
--
-- /See:/ 'mkSourceConfiguration' smart constructor.
data SourceConfiguration = SourceConfiguration'
  { -- | The name of the configuration template.
    templateName :: Lude.Maybe Lude.Text,
    -- | The name of the application associated with the configuration.
    applicationName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceConfiguration' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the configuration template.
-- * 'applicationName' - The name of the application associated with the configuration.
mkSourceConfiguration ::
  SourceConfiguration
mkSourceConfiguration =
  SourceConfiguration'
    { templateName = Lude.Nothing,
      applicationName = Lude.Nothing
    }

-- | The name of the configuration template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scTemplateName :: Lens.Lens' SourceConfiguration (Lude.Maybe Lude.Text)
scTemplateName = Lens.lens (templateName :: SourceConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: SourceConfiguration)
{-# DEPRECATED scTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The name of the application associated with the configuration.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scApplicationName :: Lens.Lens' SourceConfiguration (Lude.Maybe Lude.Text)
scApplicationName = Lens.lens (applicationName :: SourceConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: SourceConfiguration)
{-# DEPRECATED scApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.ToQuery SourceConfiguration where
  toQuery SourceConfiguration' {..} =
    Lude.mconcat
      [ "TemplateName" Lude.=: templateName,
        "ApplicationName" Lude.=: applicationName
      ]
