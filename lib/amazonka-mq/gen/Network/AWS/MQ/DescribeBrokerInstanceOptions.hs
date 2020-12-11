{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DescribeBrokerInstanceOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe available broker instance options.
module Network.AWS.MQ.DescribeBrokerInstanceOptions
  ( -- * Creating a request
    DescribeBrokerInstanceOptions (..),
    mkDescribeBrokerInstanceOptions,

    -- ** Request lenses
    dbioNextToken,
    dbioEngineType,
    dbioMaxResults,
    dbioHostInstanceType,
    dbioStorageType,

    -- * Destructuring the response
    DescribeBrokerInstanceOptionsResponse (..),
    mkDescribeBrokerInstanceOptionsResponse,

    -- ** Response lenses
    dbiorsNextToken,
    dbiorsBrokerInstanceOptions,
    dbiorsMaxResults,
    dbiorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeBrokerInstanceOptions' smart constructor.
data DescribeBrokerInstanceOptions = DescribeBrokerInstanceOptions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    engineType ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural,
    hostInstanceType ::
      Lude.Maybe Lude.Text,
    storageType ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBrokerInstanceOptions' with the minimum fields required to make a request.
--
-- * 'engineType' - Filter response by engine type.
-- * 'hostInstanceType' - Filter response by host instance type.
-- * 'maxResults' - The maximum number of instance options that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
-- * 'nextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
-- * 'storageType' - Filter response by storage type.
mkDescribeBrokerInstanceOptions ::
  DescribeBrokerInstanceOptions
mkDescribeBrokerInstanceOptions =
  DescribeBrokerInstanceOptions'
    { nextToken = Lude.Nothing,
      engineType = Lude.Nothing,
      maxResults = Lude.Nothing,
      hostInstanceType = Lude.Nothing,
      storageType = Lude.Nothing
    }

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbioNextToken :: Lens.Lens' DescribeBrokerInstanceOptions (Lude.Maybe Lude.Text)
dbioNextToken = Lens.lens (nextToken :: DescribeBrokerInstanceOptions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBrokerInstanceOptions)
{-# DEPRECATED dbioNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filter response by engine type.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbioEngineType :: Lens.Lens' DescribeBrokerInstanceOptions (Lude.Maybe Lude.Text)
dbioEngineType = Lens.lens (engineType :: DescribeBrokerInstanceOptions -> Lude.Maybe Lude.Text) (\s a -> s {engineType = a} :: DescribeBrokerInstanceOptions)
{-# DEPRECATED dbioEngineType "Use generic-lens or generic-optics with 'engineType' instead." #-}

-- | The maximum number of instance options that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbioMaxResults :: Lens.Lens' DescribeBrokerInstanceOptions (Lude.Maybe Lude.Natural)
dbioMaxResults = Lens.lens (maxResults :: DescribeBrokerInstanceOptions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeBrokerInstanceOptions)
{-# DEPRECATED dbioMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Filter response by host instance type.
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbioHostInstanceType :: Lens.Lens' DescribeBrokerInstanceOptions (Lude.Maybe Lude.Text)
dbioHostInstanceType = Lens.lens (hostInstanceType :: DescribeBrokerInstanceOptions -> Lude.Maybe Lude.Text) (\s a -> s {hostInstanceType = a} :: DescribeBrokerInstanceOptions)
{-# DEPRECATED dbioHostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead." #-}

-- | Filter response by storage type.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbioStorageType :: Lens.Lens' DescribeBrokerInstanceOptions (Lude.Maybe Lude.Text)
dbioStorageType = Lens.lens (storageType :: DescribeBrokerInstanceOptions -> Lude.Maybe Lude.Text) (\s a -> s {storageType = a} :: DescribeBrokerInstanceOptions)
{-# DEPRECATED dbioStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

instance Lude.AWSRequest DescribeBrokerInstanceOptions where
  type
    Rs DescribeBrokerInstanceOptions =
      DescribeBrokerInstanceOptionsResponse
  request = Req.get mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBrokerInstanceOptionsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "brokerInstanceOptions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "maxResults")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeBrokerInstanceOptions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeBrokerInstanceOptions where
  toPath = Lude.const "/v1/broker-instance-options"

instance Lude.ToQuery DescribeBrokerInstanceOptions where
  toQuery DescribeBrokerInstanceOptions' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "engineType" Lude.=: engineType,
        "maxResults" Lude.=: maxResults,
        "hostInstanceType" Lude.=: hostInstanceType,
        "storageType" Lude.=: storageType
      ]

-- | /See:/ 'mkDescribeBrokerInstanceOptionsResponse' smart constructor.
data DescribeBrokerInstanceOptionsResponse = DescribeBrokerInstanceOptionsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    brokerInstanceOptions ::
      Lude.Maybe
        [BrokerInstanceOption],
    maxResults ::
      Lude.Maybe
        Lude.Natural,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBrokerInstanceOptionsResponse' with the minimum fields required to make a request.
--
-- * 'brokerInstanceOptions' - List of available broker instance options.
-- * 'maxResults' - Required. The maximum number of instance options that can be returned per page (20 by default). This value must be an integer from 5 to 100.
-- * 'nextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
-- * 'responseStatus' - The response status code.
mkDescribeBrokerInstanceOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBrokerInstanceOptionsResponse
mkDescribeBrokerInstanceOptionsResponse pResponseStatus_ =
  DescribeBrokerInstanceOptionsResponse'
    { nextToken = Lude.Nothing,
      brokerInstanceOptions = Lude.Nothing,
      maxResults = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiorsNextToken :: Lens.Lens' DescribeBrokerInstanceOptionsResponse (Lude.Maybe Lude.Text)
dbiorsNextToken = Lens.lens (nextToken :: DescribeBrokerInstanceOptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBrokerInstanceOptionsResponse)
{-# DEPRECATED dbiorsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of available broker instance options.
--
-- /Note:/ Consider using 'brokerInstanceOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiorsBrokerInstanceOptions :: Lens.Lens' DescribeBrokerInstanceOptionsResponse (Lude.Maybe [BrokerInstanceOption])
dbiorsBrokerInstanceOptions = Lens.lens (brokerInstanceOptions :: DescribeBrokerInstanceOptionsResponse -> Lude.Maybe [BrokerInstanceOption]) (\s a -> s {brokerInstanceOptions = a} :: DescribeBrokerInstanceOptionsResponse)
{-# DEPRECATED dbiorsBrokerInstanceOptions "Use generic-lens or generic-optics with 'brokerInstanceOptions' instead." #-}

-- | Required. The maximum number of instance options that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiorsMaxResults :: Lens.Lens' DescribeBrokerInstanceOptionsResponse (Lude.Maybe Lude.Natural)
dbiorsMaxResults = Lens.lens (maxResults :: DescribeBrokerInstanceOptionsResponse -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeBrokerInstanceOptionsResponse)
{-# DEPRECATED dbiorsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiorsResponseStatus :: Lens.Lens' DescribeBrokerInstanceOptionsResponse Lude.Int
dbiorsResponseStatus = Lens.lens (responseStatus :: DescribeBrokerInstanceOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBrokerInstanceOptionsResponse)
{-# DEPRECATED dbiorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
