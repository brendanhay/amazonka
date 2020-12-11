{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a request to compile the specified type of information of the deployed environment.
--
-- Setting the @InfoType@ to @tail@ compiles the last lines from the application server log files of every Amazon EC2 instance in your environment.
-- Setting the @InfoType@ to @bundle@ compresses the application server log files for every Amazon EC2 instance into a @.zip@ file. Legacy and .NET containers do not support bundle logs.
-- Use 'RetrieveEnvironmentInfo' to obtain the set of logs.
-- Related Topics
--
--     * 'RetrieveEnvironmentInfo'
module Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
  ( -- * Creating a request
    RequestEnvironmentInfo (..),
    mkRequestEnvironmentInfo,

    -- ** Request lenses
    reiEnvironmentName,
    reiEnvironmentId,
    reiInfoType,

    -- * Destructuring the response
    RequestEnvironmentInfoResponse (..),
    mkRequestEnvironmentInfoResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to retrieve logs from an environment and store them in your Elastic Beanstalk storage bucket.
--
-- /See:/ 'mkRequestEnvironmentInfo' smart constructor.
data RequestEnvironmentInfo = RequestEnvironmentInfo'
  { environmentName ::
      Lude.Maybe Lude.Text,
    environmentId :: Lude.Maybe Lude.Text,
    infoType :: EnvironmentInfoType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RequestEnvironmentInfo' with the minimum fields required to make a request.
--
-- * 'environmentId' - The ID of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an @InvalidParameterValue@ error.
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
-- * 'environmentName' - The name of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an @InvalidParameterValue@ error.
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
-- * 'infoType' - The type of information to request.
mkRequestEnvironmentInfo ::
  -- | 'infoType'
  EnvironmentInfoType ->
  RequestEnvironmentInfo
mkRequestEnvironmentInfo pInfoType_ =
  RequestEnvironmentInfo'
    { environmentName = Lude.Nothing,
      environmentId = Lude.Nothing,
      infoType = pInfoType_
    }

-- | The name of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an @InvalidParameterValue@ error.
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiEnvironmentName :: Lens.Lens' RequestEnvironmentInfo (Lude.Maybe Lude.Text)
reiEnvironmentName = Lens.lens (environmentName :: RequestEnvironmentInfo -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: RequestEnvironmentInfo)
{-# DEPRECATED reiEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The ID of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an @InvalidParameterValue@ error.
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiEnvironmentId :: Lens.Lens' RequestEnvironmentInfo (Lude.Maybe Lude.Text)
reiEnvironmentId = Lens.lens (environmentId :: RequestEnvironmentInfo -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: RequestEnvironmentInfo)
{-# DEPRECATED reiEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The type of information to request.
--
-- /Note:/ Consider using 'infoType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiInfoType :: Lens.Lens' RequestEnvironmentInfo EnvironmentInfoType
reiInfoType = Lens.lens (infoType :: RequestEnvironmentInfo -> EnvironmentInfoType) (\s a -> s {infoType = a} :: RequestEnvironmentInfo)
{-# DEPRECATED reiInfoType "Use generic-lens or generic-optics with 'infoType' instead." #-}

instance Lude.AWSRequest RequestEnvironmentInfo where
  type Rs RequestEnvironmentInfo = RequestEnvironmentInfoResponse
  request = Req.postQuery elasticBeanstalkService
  response = Res.receiveNull RequestEnvironmentInfoResponse'

instance Lude.ToHeaders RequestEnvironmentInfo where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RequestEnvironmentInfo where
  toPath = Lude.const "/"

instance Lude.ToQuery RequestEnvironmentInfo where
  toQuery RequestEnvironmentInfo' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RequestEnvironmentInfo" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "EnvironmentName" Lude.=: environmentName,
        "EnvironmentId" Lude.=: environmentId,
        "InfoType" Lude.=: infoType
      ]

-- | /See:/ 'mkRequestEnvironmentInfoResponse' smart constructor.
data RequestEnvironmentInfoResponse = RequestEnvironmentInfoResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RequestEnvironmentInfoResponse' with the minimum fields required to make a request.
mkRequestEnvironmentInfoResponse ::
  RequestEnvironmentInfoResponse
mkRequestEnvironmentInfoResponse = RequestEnvironmentInfoResponse'
