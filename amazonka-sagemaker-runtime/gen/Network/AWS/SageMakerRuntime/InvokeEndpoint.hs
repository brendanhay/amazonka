{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMakerRuntime.InvokeEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- After you deploy a model into production using Amazon SageMaker hosting services, your client applications use this API to get inferences from the model hosted at the specified endpoint.
--
--
-- For an overview of Amazon SageMaker, see <http://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html How It Works> .
--
-- Amazon SageMaker strips all POST headers except those supported by the API. Amazon SageMaker might add additional headers. You should not rely on the behavior of headers outside those enumerated in the request syntax.
--
-- Cals to @InvokeEndpoint@ are authenticated by using AWS Signature Version 4. For information, see <http://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html Authenticating Requests (AWS Signature Version 4)> in the /Amazon S3 API Reference/ .
--
module Network.AWS.SageMakerRuntime.InvokeEndpoint
    (
    -- * Creating a Request
      invokeEndpoint
    , InvokeEndpoint
    -- * Request Lenses
    , ieAccept
    , ieCustomAttributes
    , ieContentType
    , ieEndpointName
    , ieBody

    -- * Destructuring the Response
    , invokeEndpointResponse
    , InvokeEndpointResponse
    -- * Response Lenses
    , iersInvokedProductionVariant
    , iersCustomAttributes
    , iersContentType
    , iersResponseStatus
    , iersBody
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMakerRuntime.Types
import Network.AWS.SageMakerRuntime.Types.Product

-- | /See:/ 'invokeEndpoint' smart constructor.
data InvokeEndpoint = InvokeEndpoint'
  { _ieAccept           :: !(Maybe Text)
  , _ieCustomAttributes :: !(Maybe (Sensitive Text))
  , _ieContentType      :: !(Maybe Text)
  , _ieEndpointName     :: !Text
  , _ieBody             :: !ByteString
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'InvokeEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ieAccept' - The desired MIME type of the inference in the response.
--
-- * 'ieCustomAttributes' -
--
-- * 'ieContentType' - The MIME type of the input data in the request body.
--
-- * 'ieEndpointName' - The name of the endpoint that you specified when you created the endpoint using the <http://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint> API.
--
-- * 'ieBody' - Provides input data, in the format specified in the @ContentType@ request header. Amazon SageMaker passes all of the data in the body to the model.  For information about the format of the request body, see <http://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats
