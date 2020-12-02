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
-- Module      : Network.AWS.SWF.DescribeDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified domain, including description and status.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
module Network.AWS.SWF.DescribeDomain
    (
    -- * Creating a Request
      describeDomain
    , DescribeDomain
    -- * Request Lenses
    , ddName

    -- * Destructuring the Response
    , describeDomainResponse
    , DescribeDomainResponse
    -- * Response Lenses
    , ddrsResponseStatus
    , ddrsDomainInfo
    , ddrsConfiguration
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'describeDomain' smart constructor.
newtype DescribeDomain = DescribeDomain'
  { _ddName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddName' - The name of the domain to describe.
describeDomain
    :: Text -- ^ 'ddName'
    -> DescribeDomain
describeDomain pName_ = DescribeDomain' {_ddName = pName_}


-- | The name of the domain to describe.
ddName :: Lens' DescribeDomain Text
ddName = lens _ddName (\ s a -> s{_ddName = a})

instance AWSRequest DescribeDomain where
        type Rs DescribeDomain = DescribeDomainResponse
        request = postJSON swf
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDomainResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "domainInfo") <*>
                     (x .:> "configuration"))

instance Hashable DescribeDomain where

instance NFData DescribeDomain where

instance ToHeaders DescribeDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.DescribeDomain" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DescribeDomain where
        toJSON DescribeDomain'{..}
          = object (catMaybes [Just ("name" .= _ddName)])

instance ToPath DescribeDomain where
        toPath = const "/"

instance ToQuery DescribeDomain where
        toQuery = const mempty

-- | Contains details of a domain.
--
--
--
-- /See:/ 'describeDomainResponse' smart constructor.
data DescribeDomainResponse = DescribeDomainResponse'
  { _ddrsResponseStatus :: !Int
  , _ddrsDomainInfo     :: !DomainInfo
  , _ddrsConfiguration  :: !DomainConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsResponseStatus' - -- | The response status code.
--
-- * 'ddrsDomainInfo' - The basic information about a domain, such as its name, status, and description.
--
-- * 'ddrsConfiguration' - The domain configuration. Currently, this includes only the domain's retention period.
describeDomainResponse
    :: Int -- ^ 'ddrsResponseStatus'
    -> DomainInfo -- ^ 'ddrsDomainInfo'
    -> DomainConfiguration -- ^ 'ddrsConfiguration'
    -> DescribeDomainResponse
describeDomainResponse pResponseStatus_ pDomainInfo_ pConfiguration_ =
  DescribeDomainResponse'
    { _ddrsResponseStatus = pResponseStatus_
    , _ddrsDomainInfo = pDomainInfo_
    , _ddrsConfiguration = pConfiguration_
    }


-- | -- | The response status code.
ddrsResponseStatus :: Lens' DescribeDomainResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\ s a -> s{_ddrsResponseStatus = a})

-- | The basic information about a domain, such as its name, status, and description.
ddrsDomainInfo :: Lens' DescribeDomainResponse DomainInfo
ddrsDomainInfo = lens _ddrsDomainInfo (\ s a -> s{_ddrsDomainInfo = a})

-- | The domain configuration. Currently, this includes only the domain's retention period.
ddrsConfiguration :: Lens' DescribeDomainResponse DomainConfiguration
ddrsConfiguration = lens _ddrsConfiguration (\ s a -> s{_ddrsConfiguration = a})

instance NFData DescribeDomainResponse where
