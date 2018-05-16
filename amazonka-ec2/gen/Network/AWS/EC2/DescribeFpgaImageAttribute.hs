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
-- Module      : Network.AWS.EC2.DescribeFpgaImageAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified Amazon FPGA Image (AFI).
--
--
module Network.AWS.EC2.DescribeFpgaImageAttribute
    (
    -- * Creating a Request
      describeFpgaImageAttribute
    , DescribeFpgaImageAttribute
    -- * Request Lenses
    , dfiaDryRun
    , dfiaFpgaImageId
    , dfiaAttribute

    -- * Destructuring the Response
    , describeFpgaImageAttributeResponse
    , DescribeFpgaImageAttributeResponse
    -- * Response Lenses
    , dfiarsFpgaImageAttribute
    , dfiarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeFpgaImageAttribute' smart constructor.
data DescribeFpgaImageAttribute = DescribeFpgaImageAttribute'
  { _dfiaDryRun      :: !(Maybe Bool)
  , _dfiaFpgaImageId :: !Text
  , _dfiaAttribute   :: !FpgaImageAttributeName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFpgaImageAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfiaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dfiaFpgaImageId' - The ID of the AFI.
--
-- * 'dfiaAttribute' - The AFI attribute.
describeFpgaImageAttribute
    :: Text -- ^ 'dfiaFpgaImageId'
    -> FpgaImageAttributeName -- ^ 'dfiaAttribute'
    -> DescribeFpgaImageAttribute
describeFpgaImageAttribute pFpgaImageId_ pAttribute_ =
  DescribeFpgaImageAttribute'
    { _dfiaDryRun = Nothing
    , _dfiaFpgaImageId = pFpgaImageId_
    , _dfiaAttribute = pAttribute_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dfiaDryRun :: Lens' DescribeFpgaImageAttribute (Maybe Bool)
dfiaDryRun = lens _dfiaDryRun (\ s a -> s{_dfiaDryRun = a})

-- | The ID of the AFI.
dfiaFpgaImageId :: Lens' DescribeFpgaImageAttribute Text
dfiaFpgaImageId = lens _dfiaFpgaImageId (\ s a -> s{_dfiaFpgaImageId = a})

-- | The AFI attribute.
dfiaAttribute :: Lens' DescribeFpgaImageAttribute FpgaImageAttributeName
dfiaAttribute = lens _dfiaAttribute (\ s a -> s{_dfiaAttribute = a})

instance AWSRequest DescribeFpgaImageAttribute where
        type Rs DescribeFpgaImageAttribute =
             DescribeFpgaImageAttributeResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeFpgaImageAttributeResponse' <$>
                   (x .@? "fpgaImageAttribute") <*> (pure (fromEnum s)))

instance Hashable DescribeFpgaImageAttribute where

instance NFData DescribeFpgaImageAttribute where

instance ToHeaders DescribeFpgaImageAttribute where
        toHeaders = const mempty

instance ToPath DescribeFpgaImageAttribute where
        toPath = const "/"

instance ToQuery DescribeFpgaImageAttribute where
        toQuery DescribeFpgaImageAttribute'{..}
          = mconcat
              ["Action" =:
                 ("DescribeFpgaImageAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dfiaDryRun,
               "FpgaImageId" =: _dfiaFpgaImageId,
               "Attribute" =: _dfiaAttribute]

-- | /See:/ 'describeFpgaImageAttributeResponse' smart constructor.
data DescribeFpgaImageAttributeResponse = DescribeFpgaImageAttributeResponse'
  { _dfiarsFpgaImageAttribute :: !(Maybe FpgaImageAttribute)
  , _dfiarsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFpgaImageAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfiarsFpgaImageAttribute' - Information about the attribute.
--
-- * 'dfiarsResponseStatus' - -- | The response status code.
describeFpgaImageAttributeResponse
    :: Int -- ^ 'dfiarsResponseStatus'
    -> DescribeFpgaImageAttributeResponse
describeFpgaImageAttributeResponse pResponseStatus_ =
  DescribeFpgaImageAttributeResponse'
    { _dfiarsFpgaImageAttribute = Nothing
    , _dfiarsResponseStatus = pResponseStatus_
    }


-- | Information about the attribute.
dfiarsFpgaImageAttribute :: Lens' DescribeFpgaImageAttributeResponse (Maybe FpgaImageAttribute)
dfiarsFpgaImageAttribute = lens _dfiarsFpgaImageAttribute (\ s a -> s{_dfiarsFpgaImageAttribute = a})

-- | -- | The response status code.
dfiarsResponseStatus :: Lens' DescribeFpgaImageAttributeResponse Int
dfiarsResponseStatus = lens _dfiarsResponseStatus (\ s a -> s{_dfiarsResponseStatus = a})

instance NFData DescribeFpgaImageAttributeResponse
         where
