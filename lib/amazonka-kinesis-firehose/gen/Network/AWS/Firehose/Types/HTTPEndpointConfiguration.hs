{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointConfiguration
  ( HTTPEndpointConfiguration (..),

    -- * Smart constructor
    mkHTTPEndpointConfiguration,

    -- * Lenses
    httpecURL,
    httpecName,
    httpecAccessKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration of the HTTP endpoint to which Kinesis Firehose delivers data.
--
-- /See:/ 'mkHTTPEndpointConfiguration' smart constructor.
data HTTPEndpointConfiguration = HTTPEndpointConfiguration'
  { -- | The URL of the HTTP endpoint selected as the destination.
    url :: Lude.Sensitive Lude.Text,
    -- | The name of the HTTP endpoint selected as the destination.
    name :: Lude.Maybe Lude.Text,
    -- | The access key required for Kinesis Firehose to authenticate with the HTTP endpoint selected as the destination.
    accessKey :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPEndpointConfiguration' with the minimum fields required to make a request.
--
-- * 'url' - The URL of the HTTP endpoint selected as the destination.
-- * 'name' - The name of the HTTP endpoint selected as the destination.
-- * 'accessKey' - The access key required for Kinesis Firehose to authenticate with the HTTP endpoint selected as the destination.
mkHTTPEndpointConfiguration ::
  -- | 'url'
  Lude.Sensitive Lude.Text ->
  HTTPEndpointConfiguration
mkHTTPEndpointConfiguration pURL_ =
  HTTPEndpointConfiguration'
    { url = pURL_,
      name = Lude.Nothing,
      accessKey = Lude.Nothing
    }

-- | The URL of the HTTP endpoint selected as the destination.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpecURL :: Lens.Lens' HTTPEndpointConfiguration (Lude.Sensitive Lude.Text)
httpecURL = Lens.lens (url :: HTTPEndpointConfiguration -> Lude.Sensitive Lude.Text) (\s a -> s {url = a} :: HTTPEndpointConfiguration)
{-# DEPRECATED httpecURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The name of the HTTP endpoint selected as the destination.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpecName :: Lens.Lens' HTTPEndpointConfiguration (Lude.Maybe Lude.Text)
httpecName = Lens.lens (name :: HTTPEndpointConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: HTTPEndpointConfiguration)
{-# DEPRECATED httpecName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The access key required for Kinesis Firehose to authenticate with the HTTP endpoint selected as the destination.
--
-- /Note:/ Consider using 'accessKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpecAccessKey :: Lens.Lens' HTTPEndpointConfiguration (Lude.Maybe (Lude.Sensitive Lude.Text))
httpecAccessKey = Lens.lens (accessKey :: HTTPEndpointConfiguration -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {accessKey = a} :: HTTPEndpointConfiguration)
{-# DEPRECATED httpecAccessKey "Use generic-lens or generic-optics with 'accessKey' instead." #-}

instance Lude.ToJSON HTTPEndpointConfiguration where
  toJSON HTTPEndpointConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Url" Lude..= url),
            ("Name" Lude..=) Lude.<$> name,
            ("AccessKey" Lude..=) Lude.<$> accessKey
          ]
      )
