{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteConfigurationAggregator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified configuration aggregator and the aggregated data associated with the aggregator.
module Network.AWS.Config.DeleteConfigurationAggregator
  ( -- * Creating a request
    DeleteConfigurationAggregator (..),
    mkDeleteConfigurationAggregator,

    -- ** Request lenses
    dcaConfigurationAggregatorName,

    -- * Destructuring the response
    DeleteConfigurationAggregatorResponse (..),
    mkDeleteConfigurationAggregatorResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteConfigurationAggregator' smart constructor.
newtype DeleteConfigurationAggregator = DeleteConfigurationAggregator'
  { configurationAggregatorName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConfigurationAggregator' with the minimum fields required to make a request.
--
-- * 'configurationAggregatorName' - The name of the configuration aggregator.
mkDeleteConfigurationAggregator ::
  -- | 'configurationAggregatorName'
  Lude.Text ->
  DeleteConfigurationAggregator
mkDeleteConfigurationAggregator pConfigurationAggregatorName_ =
  DeleteConfigurationAggregator'
    { configurationAggregatorName =
        pConfigurationAggregatorName_
    }

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaConfigurationAggregatorName :: Lens.Lens' DeleteConfigurationAggregator Lude.Text
dcaConfigurationAggregatorName = Lens.lens (configurationAggregatorName :: DeleteConfigurationAggregator -> Lude.Text) (\s a -> s {configurationAggregatorName = a} :: DeleteConfigurationAggregator)
{-# DEPRECATED dcaConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

instance Lude.AWSRequest DeleteConfigurationAggregator where
  type
    Rs DeleteConfigurationAggregator =
      DeleteConfigurationAggregatorResponse
  request = Req.postJSON configService
  response = Res.receiveNull DeleteConfigurationAggregatorResponse'

instance Lude.ToHeaders DeleteConfigurationAggregator where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DeleteConfigurationAggregator" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteConfigurationAggregator where
  toJSON DeleteConfigurationAggregator' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "ConfigurationAggregatorName"
                  Lude..= configurationAggregatorName
              )
          ]
      )

instance Lude.ToPath DeleteConfigurationAggregator where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteConfigurationAggregator where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteConfigurationAggregatorResponse' smart constructor.
data DeleteConfigurationAggregatorResponse = DeleteConfigurationAggregatorResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConfigurationAggregatorResponse' with the minimum fields required to make a request.
mkDeleteConfigurationAggregatorResponse ::
  DeleteConfigurationAggregatorResponse
mkDeleteConfigurationAggregatorResponse =
  DeleteConfigurationAggregatorResponse'
